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

;; Insert the special variable this into a function closure's formal parameters
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
      ((eq? (car formal) 'this) (bind-to-layer ; If the variable is this add the instance as an actual parameter
                                 (car formal)
                                 (box next-instance)
                                 (add-params-layer (cdr formal) actual state throw type instance next-instance))) 
      ((or (null? formal) (null? actual)) (error 'mismatched_Arguments "Incorrect amount of arguments encountered"))
      (else (bind-to-layer (car formal) (box
                                         (value (car actual) state throw type instance))
                           (add-params-layer (cdr formal)
                                             (cdr actual)
                                             state throw  type instance next-instance))))))

;; Determines whether the instance found is actually an instance or a class (in the case of a static method)
(define eval-instance
  (lambda (instance)
    (if (> (length instance) 2)
        'None
        instance)))

;; Determine the class closure to look up the method body in
(define lookup-class
  (lambda (expression state class-instance type)
    (cond
      ((eq? class-instance 'None) (lookup-value (left-op (left-op expression)) state))
      ((eq? (left-op (left-op expression)) 'super) (lookup-value (class-superclass (lookup-value type state)) state))
      (else (lookup-value (instance-type class-instance) state)))))

(define lookup-instance
  (lambda (expression state throw type instance)
    (if (eq? (left-op (left-op expression)) 'super)
        instance
        (value (left-op (left-op expression)) state throw type instance))))
        

;; Evaluates a function call value in the given expression                                        
(define eval-function-call
  (lambda (expression state throw type instance)
    (let* ((class-instance (eval-instance (lookup-instance expression state throw type instance)))
           (closure (lookup-method (right-op (left-op expression)) (class-methods (lookup-class expression state class-instance type))))
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

;; Takes a class definition and adds its closure to the state
(define class-definition-state
  (lambda (expression state)
    (if (null? (caddr expression))
        (add-variable (left-op expression) (create-class-closure (car (cdddr expression)) (init-class-closure (caddr expression)) (left-op expression) state) state)
        (add-variable (left-op expression) (create-class-closure (car (cdddr expression)) (init-class-closure (cadr (caddr expression))) (left-op expression) state) state))))
        


;; Create the class closure from the definition of the class
(define create-class-closure
  (lambda (declaration closure class-name state)
    (cond
      ((and (null? declaration) (null? (class-superclass closure))) closure)
      ((null? declaration) (append-super-defs closure (lookup-value (class-superclass closure) state)))
      ((or (eq? (operator (car declaration)) 'function)
           (eq? (operator (car declaration)) 'static-function)) ; If is a method change add it to the methods in the closure
       (create-class-closure (cdr declaration)
                             (set-class-closure
                              (class-superclass closure)
                              (class-field-names closure)
                              (add-method (car declaration) (class-methods closure) class-name)
                              (class-init-fields closure))
                             class-name state))
      ((eq? (operator (car declaration)) 'var) ; If it is a variable add it to the field names and initial field values
       (create-class-closure (cdr declaration)
                             (set-class-closure
                              (class-superclass closure)
                              (cons (left-op (car declaration)) (class-field-names closure))
                              (class-methods closure)
                              (cons (right-op (car declaration)) (class-init-fields closure)))
                             class-name state))
      (else (error "Unexpected expression in class declaration")))))

;; Appends the superclass's field and method closures to the closure of the instance
(define append-super-defs
  (lambda (closure super-class)
    (set-class-closure (class-superclass closure)
                       (append (class-field-names closure) (class-field-names super-class))
                       (combine-super-methods closure super-class)
                       (append (class-init-fields closure) (class-init-fields super-class)))))

(define combine-super-methods
  (lambda (closure super-closure)
    (list (append (car (class-methods closure))
                  (car (class-methods super-closure)))
          (append (cadr (class-methods closure))
                  (cadr (class-methods super-closure))))))

; Takes the methods section of a class closure and adds a new method to it
(define add-method
  (lambda (declaration method-layer class-name)
    (car (function-definition-state declaration (list method-layer) class-name))))

;;; -------------------- This section deals with helper functions for fields --------------

;; Given a class closure and an instance closure find the value of field name
(define lookup-field
  (lambda (name class instance)
    (lookup-field-helper name (class-field-names class) (reverse (instance-values instance)))))

(define lookup-field-helper
  (lambda (name class-names instance-fields)
    (cond
      ((or (null? class-names) (null? instance-fields)) (error "Field does not exist"))
      ((eq? name (car class-names)) (unbox (car instance-fields)))
      (else (lookup-field-helper name (cdr class-names) (cdr instance-fields))))))

;; Give a class closure and an instance closure update the value of field with name to next-value
(define update-field
  (lambda (name next-value class instance)
    (update-field-helper name next-value (class-field-names class) (reverse (instance-values instance)))))

(define update-field-helper
  (lambda (name next-value class-names instance-fields)
    (cond
      ((or (null? class-names) (null? instance-fields)) (error "Field does not exist"))
      ((eq? name (car class-names)) (set-box! (car instance-fields) next-value))
      (else (update-field-helper name (cdr class-names) (cdr instance-fields))))))


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
    (if (list? (left-op expression))
        (assignment-dot-helper expression (value (left-op (left-op expression)) state throw type instance) state break continue return throw type instance)
        (assignment-state-helper expression state state break continue return throw type instance))))

;; This helper function keeps the full state from when assignment-state was called
;; so that it can be used in the value function
(define assignment-state-helper
  (lambda (expression current-state full-state break continue return throw type instance)
    (cond
      ((null? current-state) (update-field (left-op expression) (value (right-op expression) full-state throw type instance) type instance)) 
      ((is-declared (left-op expression) (var-names current-state)) (set-variable (left-op expression)
                                                                                  (value (right-op expression) full-state throw type instance)
                                                                                  current-state))
      (else (push-state-layer (state-top-layer current-state)
                              (assignment-state-helper expression (remove-state-layer current-state)
                                                       full-state break continue return throw type instance))))))

;; Determines assignment when the left operand is a dot expression
;; Requires updating the value of an instance's field
(define assignment-dot-helper
  (lambda (expression expression-instance state break continue return throw type instance)
    (update-field (right-op (left-op expression))
                  (value (right-op expression)
                         state throw type instance)
                  (lookup-value (instance-type expression-instance) state)
                  expression-instance)))

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
      ((eq? (operator expression) 'dot) (dot-value expression state throw type instance))
      ((eq? (num-operands expression) 1) (expr-one-op-val expression state throw type instance))
      (else (expr-two-op-val expression state throw type instance)))))

;; Value of a variable
;; The variable must have been initialized previously or the function will result in an error
(define variable-value
  (lambda (name state type instance)
    (variable-value-helper name state type instance state)))

(define variable-value-helper
  (lambda (name state type instance full-state)
    (cond
      ((null? state) (lookup-field name (lookup-value type full-state) instance)) ; If variable isn't found check if it's a field in the current class
      ((null? (var-names state)) (variable-value-helper name (remove-state-layer state) type instance full-state))
      ((eq? name (car (var-names state)))
       (if (eq? (unbox (car (var-values state))) 'uninitialized)
           (error 'uninitialized_variable "variable has not been initialized before use") ; Check if variable has been initialized before reeturning
           (unbox (car (var-values state)))))
      (else (variable-value-helper name (pop-state-value state) type instance full-state)))))

;; Determine the value of a new operator
;; Creates a class instance closure
(define class-instance-value
  (lambda (expression state type instance)
    (list (left-op expression) (reverse (initialize-fields (class-init-fields (lookup-value (left-op expression) state)) '())))))

;; Determine the value of a dot expression
(define dot-value
  (lambda (expression state throw type instance)
    (field-value (right-op expression) (value (left-op expression) state throw type instance) state type)))

;; Determine the value of a field with field-name
(define field-value
  (lambda (field-name instance state type)
    (lookup-field field-name (lookup-value type state) instance)))

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


