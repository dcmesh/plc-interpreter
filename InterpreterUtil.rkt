#lang racket

(provide push-state-layer)
(provide remove-state-layer)
(provide state-top-layer)
(provide init-state)
(provide init-layer)

(provide var-names)
(provide var-values)
(provide pop-state-value)
(provide push-state-value)
(provide init-layer-value)
(provide operator)
(provide left-op)
(provide right-op)
(provide operand)
(provide num-operands)
(provide is-declared)
(provide is-atom)

(provide get-try-block)
(provide get-catch-block)
(provide get-catch-error)
(provide get-finally-block)

(provide lookup-value)
(provide lookup-var-in-state)
(provide bind-to-layer)
(provide closure-environment-creator)
(provide closure-formal-params)
(provide closure-function-body)
(provide closure-compile-type)

(provide class-field-names)
(provide class-superclass)
(provide class-methods)
(provide class-init-fields)
(provide lookup-method)
(provide init-class-closure)
(provide set-class-closure)
(provide instance-type)
(provide instance-values)
(provide initialize-fields)


;;;;---------------------------------------------------------------------------------------
;;;; Utility functions that will be needed in many files
;;;; This includes functions used for abstraction and
;;;; frequently need functions to alter the state
;;;;---------------------------------------------------------------------------------------



;;; -------------------- Utility for dealing with states --------------------


;; Takes a state layer and the state
;; Returns a state with the new layer at the front
(define push-state-layer
  (lambda (layer state)
    (cons layer state)))

;; Removes a layer from the front of the state
(define remove-state-layer
  (lambda (state)
    (cdr state)))

;; Return the top most layer of the state
(define state-top-layer
  (lambda (state)
    (car state)))

;; the state that should be used when starting a program
(define init-state '((() ())))

;; the intial layer that should be added when a block is entered
(define init-layer '(() ()))


;;; -------------------- Utility for dealing with variables and expressions --------------------


;; Takes a state and returns the list of variable names in the top layer
(define var-names caar)

;; Takes a state and returns a list of variable values in the top layer
;; The ith name in var-names should correspond with the ith value in var-values
(define var-values cadar)

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

;; creates a name and a value and creates a new layer with a variable of that name and value
(define init-layer-value
  (lambda (name value)
    (list (list name) (list (box value)))))

;; To change to prefix/postfix -> swap cadr with car/caddr
(define operator
  (lambda (expression)
    (car expression)))

;; left-op gets the lefthand expression for some operation
(define left-op cadr)

;; right-op gets the righthand expression for some operation
(define right-op caddr)

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


;;; -------------------- Utility for dealing with try/catch  --------------------


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


;;; -------------------- Utility for dealing with functions  --------------------


;; Utility functions for function calls
(define list-var car)
(define first-var caar)
(define rest-var cdar)
(define list-value cadr)
(define first-value caadr)
(define rest-value cdadr)
(define first-layer car)
(define rest-layer cdr)
(define closure-environment-creator caddr)
(define closure-function-body cadr)
(define closure-formal-params car)
(define closure-compile-type cadddr)

;; checks if a given layer is empty
(define empty-layer?
  (lambda (layer)
    (null? (list-var layer))))

;; gets the layer that has the first removed binding
(define remaining-bindings
  (lambda (layer)
    (list (rest-var layer) (rest-value layer))))

;; lookup a variable in a layer
(define lookup-var-in-layer
  (lambda (var layer)
    (cond
      ((empty-layer? layer) 'undeclared)
      ((eq? var (first-var layer)) (first-value layer))
      (else (lookup-var-in-layer var (remaining-bindings layer))))))

;; checks if a variable is in a given layer
(define var-in-layer?
  (lambda (var layer)
    (not (eq? (lookup-var-in-layer var layer) 'undeclared))))

;; binds a variable-value pair, adds it to the given layer
(define bind-to-layer
  (lambda (var value layer)
    (list (cons var (list-var layer))
          (cons value (list-value layer)))))

;; lookup a variable in a state
(define lookup-var-in-state
  (lambda (var state)
    (cond
      ((null? state) 'undeclared)
      ((var-in-layer? var (first-layer state)) (lookup-var-in-layer var (first-layer state)))
      (else (lookup-var-in-state var (rest-layer state))))))

;; checks if a variable is in a given state
(define var-in-state?
  (lambda (var state)
    (not (eq? (lookup-var-in-state var state) 'undeclared))))

;; gets the box of a given variable in a state
(define get-box-state
  (lambda (var state)
    (cond
      ((not (var-in-state? var state)) (error 'undeclared-Variable "Variable is undeclared (possibly out of scope)"))
      ((eq? 'uninitialized (lookup-var-in-state var state)) (error 'uninitialized-Variable "Variable is uninitialized"))
      (else (lookup-var-in-state var state)))))

;; lookup a value of a variable in a state
(define lookup-value
  (lambda (var state)
    (unbox (get-box-state var state))))


;;; ------------------- Utility Functions for Classes ----------------
(define class-superclass car)
(define class-field-names cadr)
(define class-methods caddr)
(define class-init-fields cadddr)

;; Create the initial closure of a class definition without any add values
(define init-class-closure
  (lambda (superclass)
    (list superclass '() init-layer '())))

;; Set the class closure to a new series of values
(define set-class-closure
  (lambda (superclass fields methods init)
    (list superclass fields methods init)))

;; lookup a variable in a layer
(define lookup-method
  (lambda (name method-list)
    (cond
      ((empty-layer? method-list) 'undeclared)
      ((eq? name (first-var method-list)) (unbox (first-value method-list)))
      (else (lookup-method name (remaining-bindings method-list))))))

;;; ------------------- Utility Functions for Class Instances ----------------
(define instance-type car)
(define instance-values cadr)
           
;; Take the initialized values of fields and the current fields in an instance closure
;; And updates the instance closure fields to be the initialized values
(define initialize-fields
  (lambda (init-fields current-fields)
    (cond
      ((null? init-fields) current-fields)
      (else (initialize-fields (cdr init-fields) (cons (box (car init-fields)) current-fields))))))