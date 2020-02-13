#lang racket
(require "InterpreterUtil.rkt")
(require "InterpreterValue.rkt")

(provide updateState)
(define updateState
  (lambda (expression state)
    (cond
      ((eq? (operator expression) 'return) (returnState expression state))
      ((eq? (operator expression) 'var) (declareState expression state))
      ((eq? (operator expression) '=) (assignmentState expression state))
      (else (error 'bad_operation "The operator is not known")))))

(define addToState
  (lambda (var value state)
    (list (cons var (variablenames state)) (cons value (variablevalues state)))))

(define removeFromState
  (lambda (var state)
    (cond
      ((null? (variablenames state)) state)
      ((eq? var (car (variablenames state))) (list (cdr (variablenames state)) (cdr (variablevalues state))))
      (else (addToState (car (variablenames state)) (car (variablevalues state)) (removeFromState var (list (cdr (variablenames state)) (cdr (variablevalues state)))))))))

(provide isDeclared)
(define isDeclared
  (lambda (name variables)
    (cond
      ((null? variables) #f)
      ((eq? name (car variables)) #t)
      (else (isDeclared name (cdr variables))))))

(define declareState
  (lambda (expression state)
    (if (= (numOperands expression) 2) (addToState (leftoperand expression) (value (rightoperand expression) state) state)
    (addToState (leftoperand expression) 'uninitialized state))))

(define assignmentState
  (lambda (expression state)
    (cond
      ((isDeclared (leftoperand expression) (variablenames state)) (addToState (leftoperand expression) (value (rightoperand expression) state) (removeFromState (leftoperand expression) state)))
      (else (error 'undeclared_variable "Variable used before declared")))))

; State after a return value
(define returnState
  (lambda (expression state)
    (if (not (isDeclared 'return (variablenames state))) (addToState 'return (value (leftoperand expression) state) state)
    (state))))