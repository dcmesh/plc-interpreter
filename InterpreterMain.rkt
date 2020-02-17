#lang racket
(require "simpleParser.rkt")
(require "InterpreterState.rkt")
(require "InterpreterValue.rkt")
(require "InterpreterUtil.rkt")

;; Function to interpret a program contained in a file
;; This will call the parser and initialize the state
(define interpret
  (lambda (file)
    (run (parser file) init-state)))

;; Take a list of statements and the current state
;; Will keep evaluating statements until a return is reached in the program
(define run
  (lambda (program state)
    (cond
      ((is-declared 'return (var-names state)) (sanitize-return (value 'return state)))
      ((null? program) (error 'no_return "program end reached without a return"))
      (else (run (cdr program) (update-state (car program) state))))))

;; changes the boolean return from #t and #f to true and false
(define sanitize-return
  (lambda (expression)
    (cond
      ((eq? expression #t) 'true)
      ((eq? expression #f) 'false)
      (else expression))))