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

(provide variablenames)
(define variablenames car)

(provide variablevalues)
(define variablevalues cadr)

(provide numOperands)
(define numOperands
  (lambda (expression)
    (cond
      ((null? (cdr expression)) 0)
      (else (+ 1 (numOperands (cdr expression)))))))