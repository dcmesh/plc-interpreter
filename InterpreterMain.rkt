#lang racket
(require "InterpreterState.rkt")
(require "InterpreterValue.rkt")
(require "InterpreterUtil.rkt")

(define interpret
  (lambda (program state)
    (cond
      ((isDeclared 'return (variablenames state)) (sanitizeReturn (value 'return state)))
      ((null? program) (error 'no_return "program end reached without a return"))
      (else (interpret (cdr program) (updateState (car program) state))))))

(define sanitizeReturn
  (lambda (expression)
    (cond
      ((eq? expression #t) 'true)
      ((eq? expression #f) 'false)
      (else expression))))