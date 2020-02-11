#lang racket

;To change to prefix/postfix -> swap cadr with car/caddr
(provide operator)
(define operator
  (lambda (expression)
    (car expression)))

(provide leftoperand)
(define leftoperand cadr)
(provide rightoperand)
(define rightoperand caddr)

; Function that finds right function to interpret the value
(provide value)
(define value
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((not (pair? expression)) (variableValue expression state))
      ((eq? 'return (operator expression)) (returnValue expression state))
      (else (expressionValue expression state)))))

; Value of a variable
(define variableValue
  (lambda (name state)
    (cond
      ((null? state) (error 'unassigned_variable "variable is used before it is assigned"))
      ((eq? name (caar state)) (cadar state))
      (else (variableValue name (cdr state))))))

; Value of a return statement
(define returnValue
  (lambda (expression state)
    (value leftoperand state)))

;M_value (<value1> + <value2>, state) = M_value(<value1>, state) + M_value(<value2>,
(define expressionValue
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((eq? '+ (operator expression)) (+ (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '* (operator expression)) (* (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '- (operator expression)) (- (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '% (operator expression)) (modulo (value (leftoperand expression) state) (value (rightoperand expression) state)))
      (else (error 'bad_operation "The operator is not known")))))