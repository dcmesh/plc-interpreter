#lang racket

(require "classParser.rkt")
(require "InterpreterState.rkt")
(require "InterpreterUtil.rkt")

;;;----------------------------------------------------------------------------------
;;; Interpreter Assignment 3 for EECS 345
;;; Group 30: David Meshnick (dcm101), Austin Keppers (agk51), Trey Starshak (mjs386)
;;;----------------------------------------------------------------------------------


;; Function to interpret a program contained in a file
;; This will call the parser and initialize the state as well as continuations
(define interpret
  (lambda (file class-name)
    (sanitize-return
     (eval-function-call
      '(funcall main)
      (run-first-pass (parser file) init-state)
      (lambda (v) (error "Error: Uncaught Exception"))
      (string->symbol class-name)
      'None))))

;; First pass to find all function declarations in a program
(define run-first-pass
  (lambda (program state)
    (cond
      ((null? program) state)
      ((eq? (operator (car program)) 'class) (run-first-pass
                                              (cdr program)
                                              (class-definition-state (car program) state)))
      (else (error "Unexpected expression")))))

;; Changes the boolean return from #t and #f to true and false
(define sanitize-return
  (lambda (return-value)
    (cond
      ((eq? return-value #t) 'true)
      ((eq? return-value #f) 'false)
      (else return-value))))