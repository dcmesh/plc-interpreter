#lang racket
(require "InterpreterState.rkt")
(require "InterpreterValue.rkt")
(require "InterpreterUtil.rkt")

(define interpret
  (lambda (program state)
    (cond
      ((isDeclared 'return (variablenames state)) (value 'return state))
      ((null? program) (error 'no_return "program end reached without a return"))
      (else (interpret (cdr program) (updateState (car program) state))))))