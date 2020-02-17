#lang racket
(provide var-names)
(provide var-values)
(provide operator)
(provide left-op)
(provide right-op)
(provide operand)
(provide num-operands)
(provide is-declared)

;;; Takes a state and returns the list of variable names
(define var-names car)

;;; Takes a state and returns a list of variable values
;;; The ith name in var-names should correspond with the ith value in var-values
(define var-values cadr)

; To change to prefix/postfix -> swap cadr with car/caddr
(define operator
  (lambda (expression)
    (car expression)))

; left-op gets the lefthand expression for some operation
(define left-op cadr)

; right-op gets the righthand expression for some operation
(define right-op caddr)

; operand gets the ith expression for some operation
(define operand
  (lambda (i op)
    (if (eq? i 0)
        (car op)
        (operand (- i 1) (cdr op)))))

; num-operands calculates the number of expressions in an operation
(define num-operands
  (lambda (expression)
    (cond
      ((null? (cdr expression)) 0)
      (else (+ 1 (num-operands (cdr expression)))))))

;;; Takes a variable name and a list of variable names
;;; Returns true if the variable has previously been declared and false otherwise.
(define is-declared
  (lambda (name variables)
    (cond
      ((null? variables) #f)
      ((eq? name (car variables)) #t)
      (else (is-declared name (cdr variables))))))