#lang racket

;To change to prefix/postfix -> swap cadr with car/caddr
(provide operator)
(define operator
  (lambda (expression)
    (car expression)))

; left-op gets the lefthand expression for some operation
(provide left-op)
(define left-op cadr)

; right-op gets the righthand expression for some operation
(provide right-op)
(define right-op caddr)

; operand gets the ith expression for some operation
(provide operand)
(define operand
  (lambda (i op)
    (if (eq? i 0)
        (car op)
        (operand (- i 1) (cdr op)))))

(provide var-names)
(define var-names car)

(provide var-values)
(define var-values cadr)

; num-operands calculates the number of expressions in an operation
(provide num-operands)
(define num-operands
  (lambda (expression)
    (cond
      ((null? (cdr expression)) 0)
      (else (+ 1 (num-operands (cdr expression)))))))