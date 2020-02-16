#lang racket
(require "simpleParser.rkt")
(require "InterpreterState.rkt")
(require "InterpreterValue.rkt")
(require "InterpreterUtil.rkt")

(define interpret
  (lambda (file)
    (run (parser file) '(() ()))))

(define run
  (lambda (program state)
    (cond
      ((is-declared 'return (var-names state)) (sanitize-return (value 'return state)))
      ((null? program) (error 'no_return "program end reached without a return"))
      (else (run (cdr program) (update-state (car program) state))))))

; changes the boolean return from #t and #f to true and false
(define sanitize-return
  (lambda (expression)
    (cond
      ((eq? expression #t) 'true)
      ((eq? expression #f) 'false)
      (else expression))))