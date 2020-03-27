#lang racket
;;(require "simpleParser.rkt")
(require "functionParser.rkt")
(require "InterpreterState.rkt")
(require "InterpreterValue.rkt")
(require "InterpreterUtil.rkt")


;;;----------------------------------------------------------------------------------
;;; Interpreter Assignment 2 for EECS 345
;;; Group 30: David Meshnick (dcm101), Austin Keppers (agk51), Trey Starshak (mjs386)
;;;----------------------------------------------------------------------------------


;; Function to interpret a program contained in a file
;; This will call the parser and initialize the state as well as continuations
(define interpret
  (lambda (file)
    (sanitize-return
     (call/cc
      (lambda (return)
        (run (parser file)
             init-state
             (lambda (v) (error "Error: Invalid break encountered."))
             (lambda (v) ("Error: Invalid continue encountered."))
             return
             (lambda (v s) (error "Error: Uncaught Exception"))))))))

;; Sequentially executes the statements in a program
(define run
  (lambda (program state break continue return throw)
    (cond
      ((null? program) (error "Error: no return encountered"))
      (else (run (cdr program)
                 (update-state (car program) state break continue return throw)
                 break continue return throw)))))


;; Changes the boolean return from #t and #f to true and false
(define sanitize-return
  (lambda (return-value)
    (cond
      ((eq? return-value #t) 'true)
      ((eq? return-value #f) 'false)
      (else return-value))))