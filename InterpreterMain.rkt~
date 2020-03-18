#lang racket
(require "simpleParser.rkt")
(require "InterpreterState.rkt")
(require "InterpreterValue.rkt")
(require "InterpreterUtil.rkt")


;;;----------------------------------------------------------------------------------
;;; Interpreter Assignment 1 for EECS 345
;;; Group 30: David Meshnick (dcm101), Austin Keppers (agk51), Trey Starshak (mjs386)
;;;----------------------------------------------------------------------------------


;; Function to interpret a program contained in a file
;; This will call the parser and initialize the state
(define interpret
  (lambda (file)
    (sanitize-return
     (call/cc
      (lambda (return)
        (update-state (parser file)
                      (init-state)
                      (lambda (v) (error "Error: Invalid break encountered."))
                      (lambda (v) ("Error: Invalid continue encountered."))
                      return
                      (lambda (v s) (error "Error: Unexpected problem encountered." s)))))))) 


;; Changes the boolean return from #t and #f to true and false
(define sanitize-return
  (lambda (return-value)
    (cond
      ((eq? return-value #t) 'true)
      ((eq? return-value #f) 'false)
      ((eq? return-value 'undeclared) (error "Error: undeclared variable encountered."))
      ((eq? return-value 'uninitialized) (error "Error: uninitialized variable encountered."))
      (else return-value))))