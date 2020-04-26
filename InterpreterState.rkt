#lang racket

(provide update-state)
(provide declare-state)
(provide function-definition-state)
(provide eval-function-call)
(provide class-definition-state)
(require "InterpreterUtil.rkt")


;;;;---------------------------------------------------------------------------
;;;; Functions that take a state and return an updated state
;;;;---------------------------------------------------------------------------


;; Takes any expression and the current state and returns and updated state.
;; If the expression does not update the state the same state is returned.
(define update-state
  (lambda (expression state break continue return throw type instance)
    (cond
      ((null? expression) state)
      ((is-atom expression) state)
      ((eq? (operator expression) 'return) (return
                                            (value (left-op expression) state throw type instance)))
      ((eq? (operator expression) 'var) (declare-state expression state throw type instance))
      ((eq? (operator expression) '=) (assignment-state
                                       expression state break continue return throw type instance))
      ((eq? (operator expression) 'while) (while-state
                                           expression state break continue return throw type instance))
      ((eq? (operator expression) 'if) (if-state
                                        expression state break continue return throw type instance))
      ((eq? (operator expression) 'break) (break state))
      ((eq? (operator expression) 'throw) (throw
                                           (value (left-op expression) state throw type instance)))
      ((eq? (operator expression) 'try) (try-state
                                         expression state break continue return throw type instance))
      ((eq? (operator expression) 'continue) (continue state))
      ((eq? (operator expression) 'begin) (block-state
                                           (cdr expression)
                                           state init-layer
                                           break continue return throw type instance))
      ((eq? (operator expression) 'class) (class-definition-state expression state))
      ((eq? (operator expression) 'function) (function-definition-state expression state))
      ((eq? (operator expression) 'funcall) (begin
                                              (eval-function-call expression state throw type instance))
                                            state)
      (else (error "Unexpected expression")))))


;;; -------------------- This section deals with variables in the state --------------------


;; Takes a variable name var a value for the variable and the current state
;; Returns the state with the addition of inputted variable name and value on the top layer
(define add-variable
  (lambda (var value state)
    (cons (list (cons var (var-names state))
                (cons (box value) (var-values state)))
          (remove-state-layer state))))

;; Takes a variable name var, an update value, and the current state
;; Returns a state that is state with the value of the variable with name var updated to value
(define set-variable
  (lambda (var value state)
    (cond
      ((null? state) state)
      ((null? (var-names state)) (push-state-layer
                                  (state-top-layer state)
                                  (set-variable var (remove-state-layer state))))
      ((eq? var (car (var-names state))) (begin
                                           (set-box! (car
                                                      (var-values state)) value) state))
      (else (push-state-value (car (var-names state))
                              (car (var-values state))
                              (set-variable var value (pop-state-value state)))))))


;;; -------------------- This section deals with functions in the state --------------------


;; Takes a function and places its definition into the state
(define function-definition-state
  (lambda (expression state type)
    (add-variable (left-op expression) (add-this-param expression (create-function-closure (cddr expression) state type)) state)))

(define add-this-param
  (lambda (expression closure)
    (if (eq? (operator expression) 'static-function)
        closure
        (cons (cons 'this (closure-formal-params closure)) (cdr closure)))))

;; Creates a closure for a given function declaration and state
(define create-function-closure
  (lambda (declaration state type)
    (cond
      ((null? declaration) (list (create-function-environment (length state)) type))
      (else (cons (car declaration) (create-function-closure (cdr declaration) state type))))))

;; Creates the correct environment for a function given the current state
;; num-layers is the number of layers that should be in the environment for the function
(define create-function-environment
  (lambda (num-layers)
    (lambda (state)
      (if (eq? (length state) num-layers)
          state
          ((create-function-environment num-layers) (remove-state-layer state))))))

;; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment break continue return throw type instance)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list)
                                  (update-state (car statement-list)
                                                environment break continue return throw type instance)
                                  break continue return throw type instance))))

;; creates a new layer with the provided params
(define add-params-layer
  (lambda (formal actual state throw type instance next-instance)
    (cond
      ((and (null? formal) (null? actual)) init-layer)
      ((or (null? formal) (null? actual)) (error 'mismatched_Arguments "Incorrect amount of arguments encountered"))
      ((eq? (car formal) 'this) (bind-to-layer (car formal) next-instance) (add-params-layer (cdr formal) (cdr actual) state throw type instance next-instance))
      (else (bind-to-layer (car formal) (box
                                         (value (car actual) state throw type instance next-instance))
                           (add-params-layer (cdr formal)
                                             (cdr actual)
                                             state throw  type instance next-instance))))))

(define eval-instance
  (lambda (expression state)
    (if (eq? (operator expression) 'dot)
        (lookup-value (left-op expression) state)
        expression)))

(define lookup-class
  (lambda (expression state class-instance)
    (if (> (length class-instance) 2)
        class-instance
        (lookup-var-in-state (instance-type class-instance) state))))
        

;; Evaluates a function call value in the given expression                                        
(define eval-function-call
  (lambda (expression state throw type instance)
    (let* ((class-instance (eval-instance (left-op expression) state))
           (closure (lookup-method (right-op (left-op expression)) (class-methods (lookup-class expression state class-instance))))
           (new-state (cons
                       (add-params-layer (closure-formal-params closure) (cddr expression) state throw type instance class-instance)
                       ((closure-environment-creator closure) state))))
      (call/cc
       (lambda (new-return)
         (interpret-statement-list (closure-function-body closure) new-state
                                            (lambda (v) (error "Error: Invalid break encountered."))
                                            (lambda (v) ("Error: Invalid continue encountered."))
                                            new-return
                                            throw (closure-compile-type closure) class-instance))))))


;;; -------------------- This section deals with class closure ---------------------------
(define class-definition-state
  (lambda (expression state)
    (add-variable (left-op expression) (create-class-closure (car (cdddr expression)) (init-class-closure (caddr expression)) (left-op expression)) state)))

(define create-class-closure
  (lambda (declaration closure class-name)
    (cond
      ((null? declaration) closure)
      ((or (eq? (operator (car declaration)) 'function) (eq? (operator (car declaration)) 'static-function))
       (create-class-closure (cdr declaration)
                             (set-class-closure
                              (class-superclass closure)
                              (class-field-names closure)
                              (add-method (car declaration) (class-methods closure) class-name))
                             class-name))
      ((eq? (operator (car declaration)) 'var)
       (create-class-closure (cdr declaration)
                             (set-class-closure
                              (class-superclass closure)
                              (cons (left-op (car declaration)) (class-field-names closure))
                              (class-methods closure))
                             class-name))
      (else (error "Unexpected expression in class declaration")))))

(define add-method
  (lambda (declaration method-layer class-name)
    (car (function-definition-state declaration (list method-layer) class-name))))


;;; -------------------- This section deals with general state updates --------------------


;; Takes a declaration expression and a state and returns the resulting state
;; This will return an error if the variable has already been declared
(define declare-state
  (lambda (expression state throw type instance)
    (cond
      ((is-declared (left-op expression) (var-names state)) (error 'redefined_variable "variable is redefined"))
      ((= (num-operands expression) 2) (add-variable (left-op expression)
                                                     (value (right-op expression) state throw type instance)
                                                     state))
      (else (add-variable (left-op expression) 'uninitialized state)))))

;; Takes an assignment expression and a state and will return the updated state
;; There will be an error if the variable in the assignment statement has not been declared yet.
(define assignment-state
  (lambda (expression state break continue return throw type instance)
    (assignment-state-helper expression state state break continue return throw type instance)))

;; This helper function keeps the full state from when assignment-state was called
;; so that it can be used in the value function
(define assignment-state-helper
  (lambda (expression current-state full-state break continue return throw type instance)
    (cond
      ((null? current-state) (error 'undeclared_variable "Variable used before declared")) 
      ((is-declared (left-op expression) (var-names current-state)) (set-variable (left-op expression)
                                                                                  (value (right-op expression) full-state throw type instance)
                                                                                  current-state))
      (else (push-state-layer (state-top-layer current-state)
                              (assignment-state-helper expression (remove-state-layer current-state)
                                                       full-state break continue return throw type instance))))))

;; Takes an expression and a state, and if the left operand of the expression is true,
;; and recurse on the updated state after the right operand has been evaluated
(define while-state
  (lambda (expression state break continue return throw type instance)
    (call/cc (lambda (new-break)
              (while-state-helper
               expression state new-break continue return throw type instance)))))

;; This helper function is used so that the above function
;; can create a single new break continuation for the entire while loop
(define while-state-helper
  (lambda (expression state break continue return throw type instance)
    (if (value (left-op expression) state throw type instance)
        (while-state-helper expression
                     (call/cc (lambda (new-continue)
                                (update-state (right-op expression)
                                              state
                                              break new-continue
                                              return throw type instance)))
                     break continue return throw type instance)
        state)))

;; Takes an expression and a state, and if the left operand evaluates as true,
;; evaluate the right operand and update the state accordingly. If the left operand
;; was false and there is a third operand (else), evalute the third operand and update
;; the state accordingly. Otherwise, return the state
(define if-state
  (lambda (expression state break continue return throw type instance)
    (cond
      ((value (left-op expression) state throw type instance) (update-state (right-op expression)
                                                                            state break continue
                                                                            return throw type instance))
      ((eq? (num-operands expression) 3) (update-state (operand 3 expression)
                                                       state break continue
                                                       return throw type instance))
      (else state))))


;;; -------------------- This section deals with try/catch --------------------


;; Takes expression, state, and continuations for a try block, then adjusts continuations as needed. To do so,
;; a throw continuation is made, along with a try-catch-finally sequence, which is then evaluated.
(define try-state
  (lambda (expression state break continue return throw type instance)
    (block-state (get-finally-block expression)
                 (call/cc
                  (lambda (new-throw)
                    (catch-continuation
                     (get-try-block expression)
                     (get-catch-block expression)
                     (get-catch-error expression)
                     (push-state-layer init-layer state)
                     break continue return new-throw throw type instance)))
                 init-layer
                 break continue return throw type instance)))

;; Takes a try block, catch block and error, state, continuations, and evaluates the try block
;; and the catch block if present
(define catch-continuation
  (lambda (try catch-block catch-error state break continue return new-throw old-throw type instance)
    (block-state try state init-layer break continue return
                 (lambda (value)
                   (new-throw
                    (block-state catch-block
                                 state
                                 (init-layer-value catch-error value)
                                 break continue return old-throw type instance))))))


;;; -------------------- This section deals with blocks --------------------


;; The block state takes a list of expressions, a layer, and a state
;; The new layer is inseted onto the state, the expressions are executed,
;; and the layer is removed when the block is exited
(define block-state
  (lambda (expression state layer break continue return throw type instance)
    (block-state-helper expression
                        (push-state-layer layer state)
                        (lambda (v) (break (remove-state-layer v)))
                        (lambda (v) (continue (remove-state-layer v)))
                        return
                        (lambda (v) (throw v))
                        type instance)))

;; Recursive part of block-state
;; Used so that the layer can be added and all the continuations updated
;; only once when the state is entered
(define block-state-helper
  (lambda (expression state break continue return throw type instance)
    (cond
      ((null? expression) (remove-state-layer state))
      (else (block-state-helper (cdr expression) (update-state
                                           (car expression)
                                           state break continue return throw type instance)
                         break continue return throw type instance)))))


;;; -------------------- This section deals with determining the value of an expression --------------------


;; Function that finds right function to interpret the value
;; Takes an expression and a state and uses the state to evaluate the expression
(define value
  (lambda (expression state throw type instance)
    (cond
      ((number? expression) expression)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((not (pair? expression)) (variable-value expression state type instance))
      ((eq? (operator expression) 'funcall) (eval-function-call expression state throw type instance))
      ((eq? (operator expression) 'new) (class-instance-value expression state type instance))
      ((eq? (num-operands expression) 1) (expr-one-op-val expression state throw type instance))
      (else (expr-two-op-val expression state throw type instance)))))

;; Value of a variable
;; The variable must have been initialized previously or the function will result in an error
(define variable-value
  (lambda (name state type instance)
    (cond
      ((null? state) (error 'undeclared_variable "variable has not been declared")) ; Variable is undeclared if var-names is null
      ((null? (var-names state)) (variable-value name (remove-state-layer state)))
      ((eq? name (car (var-names state)))
       (if (eq? (unbox (car (var-values state))) 'uninitialized)
           (error 'uninitialized_variable "variable has not been initialized before use") ; Check if variable has been initialized before reeturning
           (unbox (car (var-values state)))))
      (else (variable-value name (pop-state-value state))))))

(define class-instance-value
  (lambda (expression state type instance)
    (list (left-op expression) '())))

;; The value of an operation that has only one operand
;; If the expression does not have a numerical variable the result will
;; be the expression parsed as a boolean
(define expr-one-op-val
  (lambda (expression state throw type instance)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((eq? '- (operator expression)) (- (value (left-op expression) state throw type instance)))
      ((eq? '! (operator expression)) (not (value (left-op expression) state throw type instance)))
      (else (error 'badop "The operator is not known, or type mismatch")))))

;; The numerical or boolean value of an operation that has two operands
;; If the expression does not have a numerical variable the result will
;; be the expression parsed as a boolean
(define expr-two-op-val
  (lambda (expression state throw type instance)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((eq? '+ (operator expression)) (+
                                       (value (left-op expression) state throw type instance)
                                       (value (right-op expression) state throw type instance)))
      ((eq? '* (operator expression)) (*
                                       (value (left-op expression) state throw type instance)
                                       (value (right-op expression) state throw type instance)))
      ((eq? '- (operator expression)) (-
                                       (value (left-op expression) state throw type instance)
                                       (value (right-op expression) state throw type instance)))
      ((eq? '/ (operator expression)) (quotient
                                       (value (left-op expression) state throw type instance)
                                       (value (right-op expression) state throw type instance)))
      ((eq? '% (operator expression)) (modulo
                                       (value (left-op expression) state throw type instance)
                                       (value (right-op expression) state throw type instance)))
      ((eq? '== (operator expression)) (eq?
                                        (value (left-op expression) state throw type instance)
                                        (value (right-op expression) state throw type instance)))
      ((eq? '!= (operator expression)) (not (eq?
                                             (value (left-op expression) state throw type instance)
                                             (value (right-op expression) state throw type instance))))
      ((eq? '> (operator expression)) (>
                                       (value (left-op expression) state throw type instance)
                                       (value (right-op expression) state throw type instance)))
      ((eq? '< (operator expression)) (<
                                       (value (left-op expression) state throw type instance)
                                       (value (right-op expression) state throw type instance)))
      ((eq? '<= (operator expression)) (<=
                                        (value (left-op expression) state throw type instance)
                                        (value (right-op expression) state throw type instance)))
      ((eq? '>= (operator expression)) (>=
                                        (value (left-op expression) state throw type instance)
                                        (value (right-op expression) state throw type instance)))
      ((eq? '&& (operator expression)) (and
                                        (eq? (value (left-op expression) state throw type instance) #t)
                                        (eq? (value (right-op expression) state throw type instance) #t)))
      ((eq? '|| (operator expression)) (or
                                        (eq? (value (left-op expression) state throw type instance) #t)
                                        (eq? (value (right-op expression) state throw type instance) #t)))
      (else (error 'badop "The operator is not known, or type mismatch")))))


