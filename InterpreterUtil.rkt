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
(provide operand)
(provide init-state)
(provide init-layer)
(provide num-operands)
(provide is-declared)
(provide is-atom)
(provide validate-body)
(provide validate-catch-body)
(provide validate-finally-body)
(provide get-error)


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


;; the state that should be used when starting a program
(define init-state '((() ())))

(define init-layer '(() ()))

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


;; Validates the existance of an expression's body 
(define validate-body cadr)


;; Validates the existance of an expression's catch body
(define validate-catch-body
  (lambda (expression)
    (cdr (cdaddr expression))))


;; Validates the existance of an expression's finally body
(define validate-finally-body
  (lambda (expression)
    (validate-body (cadddr expression))))


;; Gets the type of error being caught
(define get-error
  (lambda (expression)
    (caar (cdaddr expression))))