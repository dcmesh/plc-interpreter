#lang racket
(require "InterpreterUtil.rkt")
(require "InterpreterValue.rkt")


(provide update-state)
(define update-state
  (lambda (expression state)
    (cond
      ((eq? (operator expression) 'return) (return-state expression state))
      ((eq? (operator expression) 'var) (declare-state expression state))
      ((eq? (operator expression) '=) (assignment-state expression state))
      ((eq? (operator expression) 'while) (while-state expression state))
      ((eq? (operator expression) 'if) (if-state expression state))
      (else (error 'bad_operation "The operator is not known")))))

(define add-to-state
  (lambda (var value state)
    (list (cons var (var-names state)) (cons value (var-values state)))))

(define remove-from-state
  (lambda (var state)
    (cond
      ((null? (var-names state)) state)
      ((eq? var (car (var-names state))) (list (cdr (var-names state)) (cdr (var-values state))))
      (else (add-to-state (car (var-names state))
                          (car (var-values state))
                          (remove-from-state var (list (cdr (var-names state)) (cdr (var-values state)))))))))

(provide is-declared)
(define is-declared
  (lambda (name variables)
    (cond
      ((null? variables) #f)
      ((eq? name (car variables)) #t)
      (else (is-declared name (cdr variables))))))

(define declare-state
  (lambda (expression state)
    (cond
      ((is-declared (left-op expression) (var-names state)) (error 'redefined_variable "variable is redefined"))
      ((= (num-operands expression) 2) (add-to-state (left-op expression)
                                                     (value (right-op expression) state)
                                                     state))
      (else(add-to-state (left-op expression) 'uninitialized state)))))

(define assignment-state
  (lambda (expression state)
    (cond
      ((is-declared (left-op expression) (var-names state)) (add-to-state (left-op expression)
                                                                          (value (right-op expression) state)
                                                                          (remove-from-state (left-op expression) state)))
      (else (error 'undeclared_variable "Variable used before declared")))))

; State after a return value
(define return-state
  (lambda (expression state)
    (if (not (is-declared 'return (var-names state))) (add-to-state 'return
                                                                    (value (left-op expression) state)
                                                                    state)
    (state))))

; State after a while loop
(define while-state
  (lambda (expression state)
      (if (value (left-op expression) state)
          (while-state expression (update-state (right-op expression) state))
          state)))

; State after an if statement
(define if-state
  (lambda (expression state)
    (if (eq? (num-operands expression) 3)
        (if (value (left-op expression) state)
            (update-state (right-op expression) state)
            (update-state (operand 3 expression) state))
        (if (value (left-op expression) state)
          (update-state (right-op expression) state)
          state))))
    