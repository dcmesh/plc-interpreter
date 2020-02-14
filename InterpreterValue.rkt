#lang racket
(require "InterpreterUtil.rkt")

; Function that finds right function to interpret the value
(provide value)
(define value
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((not (pair? expression)) (variableValue expression state))
      ((eq? (numOperands expression) 1) (expressionOneOperandValue expression state))
      (else (expressionTwoOperandsValue expression state)))))

; Value of a variable
(define variableValue
  (lambda (name state)
    (cond
      ((null? (variablenames state)) (error 'unassigned_variable "variable has not been declared"))
      ((eq? name (car (variablenames state)))
       (if (eq? (car (variablevalues state)) 'uninitialized) (error 'uninitialized_variable "variable has not been initialized before use")
           (car (variablevalues state))))
      (else (variableValue name (list (cdr (variablenames state)) (cdr (variablevalues state))))))))

(define expressionOneOperandValue
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((eq? '- (operator expression)) (- (value (leftoperand expression) state)))
      (else (expressionBoolean expression state)))))
      

; expressionTwoOperandsValue(<value1> + <value2>, state) = expressionTwoOperandsValue(<value1>, state) + expression_value(<value2>, state)
; if expression is for comparing booleans, calls expressionBoolean function
(define expressionTwoOperandsValue
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((eq? '+ (operator expression)) (+ (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '* (operator expression)) (* (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '- (operator expression)) (- (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '% (operator expression)) (modulo (value (leftoperand expression) state) (value (rightoperand expression) state)))
      (else (expressionBoolean expression state)))))

; Returns the boolean value as determined by the operator, or error if operator not identified
(define expressionBoolean
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((eq? '== (operator expression)) (eq? (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '!= (operator expression)) (not (eq? (value (leftoperand expression) state) (value (rightoperand expression) state))))
      ((eq? '> (operator expression))  (> (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '< (operator expression))  (< (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '<= (operator expression))  (<= (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '>= (operator expression))  (>= (value (leftoperand expression) state) (value (rightoperand expression) state)))
      ((eq? '&& (operator expression)) (and (eq? (value (leftoperand expression) state) #t) (eq? (value (rightoperand expression) state) #t)))
      ((eq? '|| (operator expression)) (or (eq? (value (leftoperand expression) state) #t) (eq? (value (rightoperand expression) state) #t)))
      ((eq? '! (operator expression)) (not (value (leftoperand expression) state)))
      (else (error 'badop "The operator is not known, or type mismatch")))))
       

