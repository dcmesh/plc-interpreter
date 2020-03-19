#lang racket
(provide var-names)
(provide var-values)
(provide push-state-layer)
(provide remove-state-layer)
(provide pop-state-value)
(provide push-state-value)
(provide state-top-layer)
(provide operator)
(provide left-op)
(provide right-op)
(provide get-try-block)
(provide get-catch-block)
(provide get-catch-error)
(provide get-finally-block)
(provide operand)
(provide init-state)
(provide init-layer)
(provide init-layer-value)
(provide num-operands)
(provide is-declared)
(provide is-atom)


;;;---------------------------------------------------------
;;; Utility functions that will be needed in many files
;;; This includes functions used for abstraction and
;;; frequently need functions to alter the state
;;;---------------------------------------------------------


;; Takes a state and returns the list of variable names in the top layer
(define var-names caar)


;; Takes a state and returns a list of variable values in the top layer
;; The ith name in var-names should correspond with the ith value in var-values
(define var-values cadar)


;; Takes a state layer and the state
;; Returns a state with the new layer at the front
(define push-state-layer
  (lambda (layer state)
    (cons layer state)))


;; Removes a layer from the front of the state
(define remove-state-layer
  (lambda (state)
    (cdr state)))


;; Removes the top most value of the state
(define pop-state-value
  (lambda (state)
    (cons (list (cdr (var-names state)) (cdr (var-values state))) (remove-state-layer state))))

;; Takes a variable name var a value for the variable and the current state
;; Returns the state with the addition of inputted variable name and value
(define push-state-value
  (lambda (var value state)
    (cons (list (cons var (var-names state))
                (cons value (var-values state)))
          (remove-state-layer state))))


;; Return the top most layer of the state
(define state-top-layer
  (lambda (state)
    (car state)))


;; To change to prefix/postfix -> swap cadr with car/caddr
(define operator
  (lambda (expression)
    (car expression)))


;; left-op gets the lefthand expression for some operation
(define left-op cadr)


;; right-op gets the righthand expression for some operation
(define right-op caddr)


;; Retrieves try block in try-catch
(define get-try-block cadr)

;; Retrives catch block in try-catch
(define get-catch-block
  (lambda (expression)
    (if (null? (caddr expression))
        '()
        (caddr (caddr expression)))))

;; gets the name of the variable in a catch block
(define get-catch-error
  (lambda (expression)
    (if (null? (caddr expression))
        '()
        (caadr (caddr expression)))))

;; Retrives the finally block in a try catch statement
(define get-finally-block
  (lambda (expression)
    (if (null? (car (cdddr expression)))
        '()
        (cadar (cdddr expression)))))


;; the state that should be used when starting a program
(define init-state '((() ())))

;; the intial layer that should be added when a block is entered
(define init-layer '(() ()))

;; creates a name and a value and creates a new layer with a variable of that name and value
(define init-layer-value
  (lambda (name value) (list (list name) (list (box value)))))

;; operand gets the ith expression for some operation
(define operand
  (lambda (i op)
    (if (eq? i 0)
        (car op)
        (operand (- i 1) (cdr op)))))


;; num-operands calculates the number of operands in an expression
(define num-operands
  (lambda (expression)
    (cond
      ((null? (cdr expression)) 0)
      (else (+ 1 (num-operands (cdr expression)))))))


;; Takes a variable name and a list of variable names
;; Returns true if the variable has previously been declared and false otherwise.
(define is-declared
  (lambda (name variables)
    (cond
      ((null? variables) #f)
      ((eq? name (car variables)) #t)
      (else (is-declared name (cdr variables))))))


;; Determines whether the expression is a non-null atom
(define is-atom
  (lambda (expression)
    (and (not (pair? expression)) (not (null? expression)))))
