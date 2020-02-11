#lang racket

;Intro to interpreters

;M_value (<value1> + <value2>, state) = M_value(<value1>, state) + M_value(<value2>,
(define Mvalue
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((number? expression) expression)
      ((eq? '+ (operator expression)) (+ (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '* (operator expression)) (* (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '- (operator expression)) (- (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '% (operator expression)) (modulo (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      (else (error 'bad_operation "The operator is not known")))))


;To change to prefix/postfix -> swap cadr with car/caddr
(provide operator)
(define operator
  (lambda (expression)
    (car expression)))

(provide leftoperand)
(define leftoperand cadr)
(provide rightoperand)
(define rightoperand caddr)


(define Mbool
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((or (eq? #t expression) (eq? #f expression)) expression)
      ((eq? '== (operator expression)) (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '!= (operator expression)) (not (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state))))
      ((eq? '<= (operator expression)) (or
                                            (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state))
                                            (< (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state))))
      ((eq? '>= (operator expression)) (or
                                            (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state))
                                            (> (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state))))
      ((eq? '> (operator expression))  (> (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '< (operator expression))  (< (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '&& (operator expression)) (and (eq? (Mbool (leftoperand expression) state) #t) (eq? (Mbool (rightoperand expression) state) #t)))
      ((eq? '|| (operator expression)) (or (eq? (Mbool (leftoperand expression) state) #t) (eq? (Mbool (rightoperand expression) state) #t)))
      (else (error 'badop "The operator is not known, or type mismatch")))))
       






