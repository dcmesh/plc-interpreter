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
      ((eq? (operator expression) 'return) (return-state expression state break continue return throw))
      ((eq? (operator expression) 'var) (declare-state expression state break continue return throw))
      ((eq? (operator expression) '=) (assignment-state expression state break continue return throw))
      ((eq? (operator expression) 'while) (call/cc (lambda (new-break) while-state expression state new-break continue return throw)))
      ((eq? (operator expression) 'if) (if-state expression state break continue return throw))
      ((eq? (operator expression) 'try) (try-state expression state break continue return throw))
      ((eq? (operator expression) 'throw) (throw (right-op expression) state))
      ((eq? (operator expression) 'break) (break state))
      ((eq? (operator expression) 'continue) (continue state))
      (else state))))


;; Takes a variable name var a value for the variable and the current state
;; Returns the state with the addition of inputted variable name and value
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
                                           (set-box! (car (var-values state)) value) state))
      (else (add-variable (car (var-names state))
                          (car (var-values state))
                          (set-variable var value (pop-state-value state)))))))


;; Takes a declaration expression and a state and returns the resulting state
;; This will return an error if the variable has already been declared
(define declare-state
  (lambda (expression state break continue return throw)
    (cond
      ((is-declared (left-op expression) (var-names state)) (error 'redefined_variable "variable is redefined"))
      ((= (num-operands expression) 2) (add-variable (left-op expression)
                                                     (value (right-op expression) state)
                                                     state))
      (else (add-variable (left-op expression) 'uninitialized (update-state
                                                               (right-op expression) state break continue return throw))))))


;; Takes an assignment expression and a state and will return the updated state
;; There will be an error if the variable in the assignment statement has not been declared yet.
(define assignment-state
  (lambda (expression state break continue return throw)
    (cond
      ((null? state) (error 'undeclared_variable "Variable used before declared")) 
      ((is-declared (left-op expression) (var-names state)) (set-variable (left-op expression)
                                                                          (value (right-op expression) state)
                                                                          state))
      (else (push-state-layer (state-top-layer state)
                              (assignment-state expression (remove-state-layer state) break continue return throw))))))


;; State after a return expression
;; Adds the result of the return to the state if there is not already a return
(define return-state
  (lambda (expression state break continue return throw)
    (if (not (is-declared 'return (var-names state))) (add-variable 'return
                                                                    (value (left-op expression) state)
                                                                    state)
    (state))))


;; Takes an expression and a state, and if the left operand of the expression is true,
;; and recurse on the updated state after the right operand has been evaluated
(define while-state
  (lambda (expression state break continue return throw)
    (if (value (left-op expression) state)
        (call/cc
         (lambda (new-continue)
           (while-state (update-state (right-op expression) state
                         break new-continue
                         return throw)
           state break continue return throw)))
        state)))

;; Takes an expression and a state, and if the left operand evaluates as true,
;; evaluate the right operand and update the state accordingly. If the left operand
;; was false and there is a third operand (else), evalute the third operand and update
;; the state accordingly. Otherwise, return the state
(define if-state
  (lambda (expression state break continue return throw)
    (cond
      ((value (left-op expression) state) (update-state (right-op expression) state break continue return throw))
      ((eq? (num-operands expression) 3) (update-state (operand 3 expression) state break continue return throw))
      (else state))))


;; Checks to see what components are in try-catch-finally section,
;; and updates state on the blocks accordingly
(define try-state
  (lambda (expression state break continue return throw)
    (update-state (validate-finally-body expression) (update-state (validate-body expression) state break continue return
                                                                   (lambda (e new-state) (update-state
                                                                                          (validate-finally-body expression)
                                                                                          (catch-state (validate-catch-body expression) e (get-error expression)
                                                                                                       new-state break continue return throw)
                                                                                          break continue return throw))) break continue return throw)))

;; State after a catch block
(define catch-state
  (lambda (expression error type state break continue return throw)
    (update-state expression (add-variable type error state) break continue return throw)))