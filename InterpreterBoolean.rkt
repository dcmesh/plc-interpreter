#lang racket
(require "InterpreterValue.rkt")

; Returns the boolean value as determined by the operator
(define expressionBoolean
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((or (eq? #t expression) (eq? #f expression)))
      ((eq? '== (operator expression)) (eq? (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '!= (operator expression)) (not (eq? (value (leftoperand expression) state) (value (rightoperand expression) state))))
      ((eq? '> (operator expression))  (> (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '< (operator expression))  (< (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '<= (operator expression))  (<= (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '>= (operator expression))  (>= (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '&& (operator expression)) (and (eq? (expressionBoolean (leftoperand expression) state) #t) (eq? (expressionBoolean (rightoperand expression) state) #t)))
      ((eq? '|| (operator expression)) (or (eq? (expressionBoolean (leftoperand expression) state) #t) (eq? (expressionBoolean (rightoperand expression) state) #t)))
      ((eq? '! (operator expression)) (not (expressionBoolean (leftoperand expression) state)))
      (else (error 'badop "The operator is not known, or type mismatch")))))
       






