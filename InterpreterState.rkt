#lang racket
(provide update-state)
(require "InterpreterUtil.rkt")
(require "InterpreterValue.rkt")
;;; Functions that take a state and return an updated state

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
(define add-to-state
  (lambda (var value state)
    (list (cons var (var-names state)) (cons value (var-values state)))))

;; Takes a variable name var and the current state
;; Returns a state that is state without the variable of name var
;; If the variable does not exist the state will be returned without changes
(define remove-from-state
  (lambda (var state)
    (cond
      ((null? (var-names state)) state)
      ((eq? var (car (var-names state))) (list (cdr (var-names state)) (cdr (var-values state))))
      (else (add-to-state (car (var-names state))
                          (car (var-values state))
                          (remove-from-state var (list (cdr (var-names state)) (cdr (var-values state)))))))))

;; Takes a declaration expression and a state and returns the resulting state
;; This will return an error if the variable has already been declared
(define declare-state
  (lambda (expression state)
    (cond
      ((is-declared (left-op expression) (var-names state)) (error 'redefined_variable "variable is redefined"))
      ((= (num-operands expression) 2) (add-to-state (left-op expression)
                                                     (value (right-op expression) state)
                                                     state))
      (else (add-to-state (left-op expression) 'uninitialized state)))))

;; Takes an assignment expression and a state and will return the updated state
;; There will be an error if the variable in the assignment statement has not been declared yet.
(define assignment-state
  (lambda (expression state)
    (cond
      ((is-declared (left-op expression) (var-names state)) (add-to-state (left-op expression)
                                                                          (value (right-op expression) state)
                                                                          (remove-from-state (left-op expression) state)))
      (else (error 'undeclared_variable "Variable used before declared")))))

;; State after a return value
(define return-state
  (lambda (expression state)
    (if (not (is-declared 'return (var-names state))) (add-to-state 'return
                                                                    (value (left-op expression) state)
                                                                    state)
    (state))))

;; State after a while loop
(define while-state
  (lambda (expression state)
      (if (expr-bool (left-op expression) state)
          (while-state expression (update-state (right-op expression) state))
          state)))

;; State after an if statement
(define if-state
  (lambda (expression state)
    (cond
      ((expr-bool (left-op expression) state) (update-state (right-op expression) state))
      ((eq? (num-operands expression) 3) (update-state (operand 3 expression) state))
      (else state))))