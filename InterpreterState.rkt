#lang racket
(require "InterpreterValue.rkt")

(define assignmentState
  (lambda (expression state)
    (cond
      ((null? state) (list (list (leftoperand expression) (rightoperand expression))))
      ((eq? (leftoperand expression) (caar state)) (cons (list (leftoperand expression) (rightoperand expression)) (cdr state)))
      (else (cons (car state) (assignmentState expression (cdr state)))))))