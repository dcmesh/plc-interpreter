#lang racket
(provide update-state)
(require "InterpreterUtil.rkt")
(require "InterpreterValue.rkt")


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
      ((eq? (operator expression) 'var) (declare-state
                                         expression state break continue return throw))
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

(define create-function-closure
  (lambda (declaration state)
    (cond
      ((null? declaration) (list (create-function-environment (length state))))
      ((eq? (car declaration) 'function) (create-function-closure (cdr declaration) state))
      (else (cons (car declaration) (create-function-closure (cdr declaration) state))))))

(define create-function-environment
  (lambda (num-layers)
    (lambda (state)
      (if (eq? (length state) num-layers)
          state
          ((create-function-environment num-layers) (remove-state-layer state))))))


;; Pushes the parameters to the given environment. Also, takes care of out-of-scope variables by
;; replacing their layers with empty placeholders
(define push-param-env
  (lambda (function lis env break continue return throw)
    (bind-params (init-state)
                 env
                 (car (lookup-value function env))
                 lis function break continue return throw)))


;; Adds the variables from the given list of parameters of a function to values passed in
;; from that function, updating the environment
(define bind-params
  (lambda (new-env old-env params lis function break continue return throw)
    (cond
      ((and (null? lis) (null? params)) (cons (car new-env) (get-correct-scope old-env function)))
      ((or (null? lis) (null? params)) (error 'mismatched_Arguments "Incorrect amount of arugments encountered"))
      (else (bind-params
             (push-state-value (car params)
                               (value (car lis) old-env)
                               new-env)
             old-env
             (cdr params)
             (cdr lis)
             function break continue return throw)))))

                                           
;; Takes a declaration expression and a state and returns the resulting state
;; This will return an error if the variable has already been declared
(define declare-state
  (lambda (expression state break continue return throw)
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


