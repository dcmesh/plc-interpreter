#lang racket
(require "InterpreterUtil.rkt")

; Function that finds right function to interpret the value
(provide value)
(define value
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((not (pair? expression)) (variable-value expression state))
      ((eq? (num-operands expression) 1) (expr-one-op-val expression state))
      (else (expr-two-op-val expression state)))))

; Value of a variable
(define variable-value
  (lambda (name state)
    (cond
      ((null? (var-names state)) (error 'unassigned_variable "variable has not been declared"))
      ((eq? name (car (var-names state)))
       (if (eq? (car (var-values state)) 'uninitialized) (error 'uninitialized_variable "variable has not been initialized before use")
           (car (var-values state))))
      (else (variable-value name (list (cdr (var-names state))
                                       (cdr (var-values state))))))))

(define expr-one-op-val
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((eq? '- (operator expression)) (- (value (left-op expression) state)))
      (else (expr-bool expression state)))))
      

; expr-two-op-val(<value1> + <value2>, state) = expr-two-op-val(<value1>, state) + expression_value(<value2>, state)
; if expression is for comparing booleans, calls expr-bool function
(define expr-two-op-val
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((eq? '+ (operator expression)) (+ (value (left-op expression) state) (value (right-op expression) state)))
      ((eq? '* (operator expression)) (* (value (left-op expression) state) (value (right-op expression) state)))
      ((eq? '- (operator expression)) (- (value (left-op expression) state) (value (right-op expression) state)))
      ((eq? '/ (operator expression)) (quotient (value (left-op expression) state) (value (right-op expression) state)))
      ((eq? '% (operator expression)) (modulo (value (left-op expression) state) (value (right-op expression) state)))
      (else (expr-bool expression state)))))

; Returns the boolean value as determined by the operator, or error if operator not identified
(define expr-bool
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((eq? '== (operator expression)) (eq? (value (left-op expression) state) (value (right-op expression) state)))
      ((eq? '!= (operator expression)) (not (eq? (value (left-op expression) state) (value (right-op expression) state))))
      ((eq? '> (operator expression))  (> (value (left-op expression) state) (value (right-op expression) state)))
      ((eq? '< (operator expression))  (< (value (left-op expression) state) (value (right-op expression) state)))
      ((eq? '<= (operator expression))  (<= (value (left-op expression) state) (value (right-op expression) state)))
      ((eq? '>= (operator expression))  (>= (value (left-op expression) state) (value (right-op expression) state)))
      ((eq? '&& (operator expression)) (and (eq? (value (left-op expression) state) #t) (eq? (value (right-op expression) state) #t)))
      ((eq? '|| (operator expression)) (or (eq? (value (left-op expression) state) #t) (eq? (value (right-op expression) state) #t)))
      ((eq? '! (operator expression)) (not (value (left-op expression) state)))
      (else (error 'badop "The operator is not known, or type mismatch")))))
       

