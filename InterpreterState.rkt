#lang racket
(provide update-state)
(provide declare-state)
(provide function-definition-state)
(provide eval-function-call)
(require "InterpreterUtil.rkt")


;;;---------------------------------------------------------
;;; Functions that take a state and return an updated state
;;;---------------------------------------------------------


;; Takes any expression and the current state and returns and updated state.
;; If the expression does not update the state the same state is returned.
(define update-state
  (lambda (expression state break continue return throw)
    (cond
      ((null? expression) state)
      ((is-atom expression) state)
      ((eq? (operator expression) 'return) (return
                                            (value (left-op expression) state)))
      ((eq? (operator expression) 'var) (declare-state expression state))
      ((eq? (operator expression) '=) (assignment-state
                                       expression state break continue return throw))
      ((eq? (operator expression) 'while) (while-state
                                           expression state break continue return throw))
      ((eq? (operator expression) 'if) (if-state
                                        expression state break continue return throw))
      ((eq? (operator expression) 'break) (break state))
      ((eq? (operator expression) 'throw) (throw
                                           (value (left-op expression) state) state))
      ((eq? (operator expression) 'try) (try-state
                                         expression state break continue return throw))
      ((eq? (operator expression) 'continue) (continue state))
      ((eq? (operator expression) 'begin) (block-state
                                           (cdr expression)
                                           state init-layer
                                           break continue return throw))
      ((eq? (operator expression) 'function) (function-definition-state expression state))
      ((eq? (operator expression) 'funcall) (return (eval-function-call expression state break continue return throw)))
      (else (error "Unexpected expression")))))


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

(define function-definition-state
  (lambda (expression state)
    (add-variable (left-op expression) (create-function-closure (cddr expression) state) state)))

(define create-function-closure
  (lambda (declaration state)
    (cond
      ((null? declaration) (list (create-function-environment (length state))))
      (else (cons (car declaration) (create-function-closure (cdr declaration) state))))))

(define create-function-environment
  (lambda (num-layers)
    (lambda (state)
      (if (eq? (length state) num-layers)
          state
          ((create-function-environment num-layers) (remove-state-layer state))))))


;; -------------------- Idea for Closure -------------------------------------------------------------------
;; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment break continue return throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) (update-state (car statement-list) environment break continue return throw) break continue return throw))))

;; creates a new layer with the provided params
(define add-params-layer
  (lambda (formal actual state break continue return throw)
    (cond
      ((and (null? formal) (null? (car actual))) init-layer)
      ((or (null? formal) (null? (car actual))) (error 'mismatched_Arguments "Incorrect amount of arugments encountered"))
      (else (bind-to-layer (car formal) (box
                                         (value (caar actual) state))
                           (add-params-layer (cdr formal)
                                             (cdr actual)
                                             state break continue return throw))))))

                                         
(define eval-function-call
  (lambda (expression state break continue return throw)
    (let* ((closure (lookup-value (cadr expression) state))
           (outer-env ((caddr closure) state))
           (new-state (cons (add-params-layer (car closure) (cddr expression) state break continue return throw) outer-env)))
      (call/cc
       (lambda (new-return)
         (interpret-statement-list (cadr closure) new-state break continue new-return throw))))))

;; -------------------- Idea for Closure -------------------------------------------------------------------

                                           
;; Takes a declaration expression and a state and returns the resulting state
;; This will return an error if the variable has already been declared
(define declare-state
  (lambda (expression state)
    (cond
      ((is-declared (left-op expression) (var-names state)) (error 'redefined_variable "variable is redefined"))
      ((= (num-operands expression) 2) (add-variable (left-op expression)
                                                     (value (right-op expression) state)
                                                     state))
      (else (add-variable (left-op expression) 'uninitialized state)))))


;; Takes an assignment expression and a state and will return the updated state
;; There will be an error if the variable in the assignment statement has not been declared yet.
(define assignment-state
  (lambda (expression state break continue return throw)
    (assignment-state-helper expression state state break continue return throw)))

;; This helper function keeps the full state from when assignment-state was called
;; so that it can be used in the value function
(define assignment-state-helper
  (lambda (expression current-state full-state break continue return throw)
    (cond
      ((null? current-state) (error 'undeclared_variable "Variable used before declared")) 
      ((is-declared (left-op expression) (var-names current-state)) (set-variable (left-op expression)
                                                                                  (value (right-op expression) full-state)
                                                                                  current-state))
      (else (push-state-layer (state-top-layer current-state)
                              (assignment-state-helper expression (remove-state-layer current-state)
                                                       full-state break continue return throw))))))

;; Takes an expression and a state, and if the left operand of the expression is true,
;; and recurse on the updated state after the right operand has been evaluated
(define while-state
  (lambda (expression state break continue return throw)
    (call/cc (lambda (new-break)
              (while-state-helper
               expression state new-break continue return throw)))))

;; This helper function is used so that the above function
;; can create a single new break continuation for the entire while loop
(define while-state-helper
  (lambda (expression state break continue return throw)
    (if (value (left-op expression) state)
        (while-state-helper expression
                     (call/cc (lambda (new-continue)
                                (update-state (right-op expression)
                                              state
                                              break new-continue
                                              return throw)))
                     break continue return throw)
        state)))

;; Takes an expression and a state, and if the left operand evaluates as true,
;; evaluate the right operand and update the state accordingly. If the left operand
;; was false and there is a third operand (else), evalute the third operand and update
;; the state accordingly. Otherwise, return the state
(define if-state
  (lambda (expression state break continue return throw)
    (cond
      ((value (left-op expression) state) (update-state (right-op expression)
                                                        state break continue
                                                        return throw))
      ((eq? (num-operands expression) 3) (update-state (operand 3 expression)
                                                       state break continue
                                                       return throw))
      (else state))))


;; Takes expression, state, and continuations for a try block, then adjusts continuations as needed. To do so,
;; a throw continuation is made, along with a try-catch-finally sequence, which is then evaluated.
(define try-state
  (lambda (expression state break continue return throw)
    (block-state (get-finally-block expression)
                 (call/cc
                  (lambda (new-throw)
                    (catch-continuation
                     (get-try-block expression)
                     (get-catch-block expression)
                     (get-catch-error expression)
                     (push-state-layer init-layer state)
                     break continue return new-throw throw)))
                 init-layer
                 break continue return throw)))


;; Takes a try block, catch block and error, state, continuations, and evaluates the try block
;; and the catch block if present
(define catch-continuation
  (lambda (try catch-block catch-error state break continue return new-throw old-throw)
    (block-state try state init-layer break continue return
                 (lambda (value throw-state)
                   (new-throw
                    (block-state catch-block
                                 throw-state
                                 (init-layer-value catch-error value)
                                 break continue return old-throw))))))

;; The block state takes a list of expressions, a layer, and a state
;; The new layer is inseted onto the state, the expressions are executed,
;; and the layer is removed when the block is exited
(define block-state
  (lambda (expression state layer break continue return throw)
    (block-state-helper expression
                        (push-state-layer layer state)
                        (lambda (v) (break (remove-state-layer v)))
                        (lambda (v) (continue (remove-state-layer v)))
                        return
                        (lambda (v s) (throw v (remove-state-layer s))))))

;; Recursive part of block-state
;; Used so that the layer can be added and all the continuations updated
;; only once when the state is entered
(define block-state-helper
  (lambda (expression state break continue return throw)
    (cond
      ((null? expression) (remove-state-layer state))
      (else (block-state-helper (cdr expression) (update-state
                                           (car expression)
                                           state break continue return throw)
                         break continue return throw)))))


;;;---------------------------------------------------------
;;; Functions for parsing expressions for values or booleans
;;;---------------------------------------------------------


;; Function that finds right function to interpret the value
;; Takes an expression and a state and uses the state to evaluate the expression
(define value
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((not (pair? expression)) (variable-value expression state))
      ((eq? (num-operands expression) 1) (expr-one-op-val expression state))
      (else (expr-two-op-val expression state)))))


;; Value of a variable
;; The variable must have been initialized previously or the function will result in an error
(define variable-value
  (lambda (name state)
    (cond
      ((null? state) (error 'undeclared_variable "variable has not been declared")) ; Variable is undeclared if var-names is null
      ((null? (var-names state)) (variable-value name (remove-state-layer state)))
      ((eq? name (car (var-names state)))
       (if (eq? (unbox (car (var-values state))) 'uninitialized)
           (error 'uninitialized_variable "variable has not been initialized before use") ; Check if variable has been initialized before reeturning
           (unbox (car (var-values state)))))
      (else (variable-value name (pop-state-value state))))))


;; The value of an operation that has only one operand
;; If the expression does not have a numerical variable the result will
;; be the expression parsed as a boolean
(define expr-one-op-val
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((eq? '- (operator expression)) (- (value (left-op expression) state)))
      ((eq? '! (operator expression)) (not (value (left-op expression) state)))
      ((eq? 'funcall (operator expression)) (function-value expression state))
      (else (error 'badop "The operator is not known, or type mismatch")))))


;; The numerical or boolean value of an operation that has two operands
;; If the expression does not have a numerical variable the result will
;; be the expression parsed as a boolean
(define expr-two-op-val
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((eq? '+ (operator expression)) (+
                                       (value (left-op expression) state)
                                       (value (right-op expression) state)))
      ((eq? '* (operator expression)) (*
                                       (value (left-op expression) state)
                                       (value (right-op expression) state)))
      ((eq? '- (operator expression)) (-
                                       (value (left-op expression) state)
                                       (value (right-op expression) state)))
      ((eq? '/ (operator expression)) (quotient
                                       (value (left-op expression) state)
                                       (value (right-op expression) state)))
      ((eq? '% (operator expression)) (modulo
                                       (value (left-op expression) state)
                                       (value (right-op expression) state)))
      ((eq? '== (operator expression)) (eq?
                                        (value (left-op expression) state)
                                        (value (right-op expression) state)))
      ((eq? '!= (operator expression)) (not (eq?
                                             (value (left-op expression) state)
                                             (value (right-op expression) state))))
      ((eq? '> (operator expression)) (>
                                       (value (left-op expression) state)
                                       (value (right-op expression) state)))
      ((eq? '< (operator expression)) (<
                                       (value (left-op expression) state)
                                       (value (right-op expression) state)))
      ((eq? '<= (operator expression)) (<=
                                        (value (left-op expression) state)
                                        (value (right-op expression) state)))
      ((eq? '>= (operator expression)) (>=
                                        (value (left-op expression) state)
                                        (value (right-op expression) state)))
      ((eq? '&& (operator expression)) (and
                                        (eq? (value (left-op expression) state) #t)
                                        (eq? (value (right-op expression) state) #t)))
      ((eq? '|| (operator expression)) (or
                                        (eq? (value (left-op expression) state) #t)
                                        (eq? (value (right-op expression) state) #t)))
      ((eq? (operator expression) 'funcall) (function-value expression state))
      (else (error 'badop "The operator is not known, or type mismatch")))))

(define function-value
  (lambda (expression state)
    (value (cdr state) state)))


