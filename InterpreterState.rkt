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
  (lambda (expression state)
    (cond
      ((eq? (operator expression) 'return) (return-state expression state))
      ((eq? (operator expression) 'var) (declare-state expression state))
      ((eq? (operator expression) '=) (assignment-state expression state))
      ((eq? (operator expression) 'while) (while-state expression state))
      ((eq? (operator expression) 'if) (if-state expression state))
      (else state))))

;; Takes a variable name var a value for the variable and the current state
;; Returns the state with the addition of inputted variable name and value
(define add-variable
  (lambda (var value state)
    (cons (list (cons var (var-names state)) (cons (box value) (var-values state))) (remove-state-layer state))))

;; Takes a variable name var and the current state
;; Returns a state that is state without the variable of name var
;; If the variable does not exist the state will be returned without changes
(define remove-variable
  (lambda (var state)
    (cond
      ((null? state) state)
      ((null? (var-names state)) (push-state-layer (state-top-layer state) (remove-variable var (remove-state-layer state))))
      ((eq? var (car (var-names state))) (push-state-layer (list (cdr (var-names state)) (cdr (var-values state))) (remove-state-layer state)))
      (else (add-variable (car (var-names state))
                          (car (var-values state))
                          (remove-variable var (pop-state-value state)))))))

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
  (lambda (expression state)
    (cond
      ((null? state) (error 'undeclared_variable "Variable used before declared")) 
      ((is-declared (left-op expression) (var-names state)) (add-variable (left-op expression)
                                                                          (value (right-op expression) state)
                                                                          (remove-variable (left-op expression) state)))
      (else (push-state-layer (state-top-layer state) (assignment-state expression (remove-state-layer state)))))))

;; State after a return expression
;; Adds the result of the return to the state if there is not already a return
(define return-state
  (lambda (expression state)
    (if (not (is-declared 'return (var-names state))) (add-variable 'return
                                                                    (value (left-op expression) state)
                                                                    state)
    (state))))

;; Takes an expression and a state, and if the left operand of the expression is true,
;; and recurse on the updated state after the right operand has been evaluated
(define while-state
  (lambda (expression state)
      (if (expr-bool (left-op expression) state)
          (while-state expression (update-state (right-op expression) state))
          state)))

;; Takes an expression and a state, and if the left operand evaluates as true,
;; evaluate the right operand and update the state accordingly. If the left operand
;; was false and there is a third operand (else), evalute the third operand and update
;; the state accordingly. Otherwise, return the state
(define if-state
  (lambda (expression state)
    (cond
      ((expr-bool (left-op expression) state) (update-state (right-op expression) state))
      ((eq? (num-operands expression) 3) (update-state (operand 3 expression) state))
      (else state))))