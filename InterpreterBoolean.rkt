#lang racket
(require "InterpreterValue.rkt")
;Intro to interpreters




(define Mbool
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((or (eq? #t expression) (eq? #f expression)))
      ((eq? '== (operator expression)) (eq? (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '!= (operator expression)) (not (eq? (value (leftoperand expression) state) (value (rightoperand expression) state))))
      ((eq? '<= (operator expression)) (or
                                            (eq? (value (leftoperand expression) state) (value (rightoperand expression) state))
                                            (< (value (leftoperand expression) state) (value (rightoperand expression) state))))
      ((eq? '>= (operator expression)) (or
                                            (eq? (value (leftoperand expression) state) (value (rightoperand expression) state))
                                            (> (value (leftoperand expression) state) (value (rightoperand expression) state))))
      ((eq? '> (operator expression))  (> (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '< (operator expression))  (< (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '&& (operator expression)) (and (eq? (Mbool (leftoperand expression) state) #t) (eq? (Mbool (rightoperand expression) state) #t)))
      ((eq? '|| (operator expression)) (or (eq? (Mbool (leftoperand expression) state) #t) (eq? (Mbool (rightoperand expression) state) #t)))
      (else (error 'badop "The operator is not known, or type mismatch")))))
       






