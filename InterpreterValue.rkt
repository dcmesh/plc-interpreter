#lang racket
(provide value)
(require "InterpreterUtil.rkt")


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

    