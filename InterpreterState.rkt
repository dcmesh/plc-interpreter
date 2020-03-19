#lang racket
(provide update-state)
(require "InterpreterUtil.rkt")
(require "InterpreterValue.rkt")


;;;---------------------------------------------------------
;;; Functions that take a state and return an updated state
;;;---------------------------------------------------------


;; Takes any expression and the current state and returns and updated state.
;; If the expression does not update the state the same state is returned.
(define update-state
  (lambda (expression state break continue return throw)
    (cond
      ((null? expression) state)
      ((is-atom expression) state)
      ((eq? (operator expression) 'return) (return (value (left-op expression) state)))
      ((eq? (operator expression) 'var) (declare-state expression state break continue return throw))
      ((eq? (operator expression) '=) (assignment-state expression state break continue return throw))
      ((eq? (operator expression) 'while) (call/cc
                                           (lambda (new-break)
                                             (while-state expression state new-break continue return throw))))
      ((eq? (operator expression) 'if) (if-state expression state break continue return throw))
      ((eq? (operator expression) 'break) (break state))
      ((eq? (operator expression) 'throw) (throw (left-op expression) state throw))
      ((eq? (operator expression) 'try) (try-state expression state break continue return throw))
      ((eq? (operator expression) 'continue) (continue state))
      ((eq? (operator expression) 'begin) (block-state
                                           (cdr expression)
                                           (push-state-layer init-layer state)
                                           break continue return throw))
      (else (error "Unexpected expression")))))


;; Takes a variable name var a value for the variable and the current state
;; Returns the state with the addition of inputted variable name and value
(define add-variable
  (lambda (var value state)
    (cons (list (cons var (var-names state))
                (cons (box value) (var-values state)))
          (remove-state-layer state))))


;; Takes a variable name var, an update value, and the current state
;; Returns a state that is state with the value of the variable with name var updated to value
(define set-variable
  (lambda (var value state)
    (cond
      ((null? state) state)
      ((null? (var-names state)) (push-state-layer
                                  (state-top-layer state)
                                  (set-variable var (remove-state-layer state))))
      ((eq? var (car (var-names state))) (begin
                                           (set-box! (car
                                                      (var-values state)) value) state))
      (else (push-state-value (car (var-names state))
                              (car (var-values state))
                              (set-variable var value (pop-state-value state)))))))


;; Takes a declaration expression and a state and returns the resulting state
;; This will return an error if the variable has already been declared
(define declare-state
  (lambda (expression state break continue return throw)
    (cond
      ((is-declared (left-op expression) (var-names state)) (error 'redefined_variable "variable is redefined"))
      ((= (num-operands expression) 2) (add-variable (left-op expression)
                                                     (value (right-op expression) state)
                                                     state))
      (else (add-variable (left-op expression) 'uninitialized state)))))


;; Takes an assignment expression and a state and will return the updated state
;; There will be an error if the variable in the assignment statement has not been declared yet.
(define assignment-state
  (lambda (expression state break continue return throw)
    (assignment-state-helper expression state state break continue return throw)))

(define assignment-state-helper
  (lambda (expression current-state full-state break continue return throw)
    (cond
      ((null? current-state) (error 'undeclared_variable "Variable used before declared")) 
      ((is-declared (left-op expression) (var-names current-state)) (set-variable (left-op expression)
                                                                                  (value (right-op expression) full-state)
                                                                                  current-state))
      (else (push-state-layer (state-top-layer current-state)
                              (assignment-state-helper expression (remove-state-layer current-state)
                                                       full-state break continue return throw))))))

;; Takes an expression and a state, and if the left operand of the expression is true,
;; and recurse on the updated state after the right operand has been evaluated
(define while-state
  (lambda (expression state break continue return throw)
    (if (value (left-op expression) state)
        (while-state expression
                     (call/cc (lambda (new-continue)
                                (update-state (right-op expression) state
                                              break new-continue
                                              return throw)))
                     break continue return throw)
        state)))

;; Takes an expression and a state, and if the left operand evaluates as true,
;; evaluate the right operand and update the state accordingly. If the left operand
;; was false and there is a third operand (else), evalute the third operand and update
;; the state accordingly. Otherwise, return the state
(define if-state
  (lambda (expression state break continue return throw)
    (cond
      ((value (left-op expression) state) (update-state (right-op expression)
                                                        state break continue
                                                        return throw))
      ((eq? (num-operands expression) 3) (update-state (operand 3 expression)
                                                       state break continue
                                                       return throw))
      (else state))))


;; Takes expression, state, and continuations for a try block, then adjusts continuations as needed. To do so,
;; a throw continuation is made, along with a try-catch-finally sequence, which is then evaluated.
(define try-state
  (lambda (expression state break continue return throw)
    (block-state (get-finally-block expression)
                 (push-state-layer init-layer
                             (call/cc
                              (lambda (new-throw)
                                (catch-continuation
                                 (get-try-block expression)
                                 (lambda (v) (block-state (get-catch-block expression) (add-variable (get-catch-error expression) v (push-state-layer  init-layer state))))
                                 (push-state-layer init-layer state)
                                 break continue return new-throw))))
                             break continue return throw)))


;; Takes a catch block, state, continuations, and a finally block. Evaluates a catch block if encountered,
;; binds and throws the error accordingly, then evaluates the given finally block
(define catch-continuation
  (lambda (try catch-cont state break continue return throw)
    (block-state try state break continue return catch-cont)))

;; Copy of run function from InterpreterMain, used here to prevent circular dependency
;; NOTE FOR LATER: Probably a good idea to combine all but InterpreterUtil functions into a single file, eliminate things like this
(define run-helper
  (lambda (program state break continue return throw)
    (cond
      ((null? program) (error "Error: no return encountered"))
      (else (run-helper (cdr program) (update-state
                                       (car program)
                                       state break continue return throw)
                        break continue return throw)))))
              

(define block-state
  (lambda (expression state break continue return throw)
    (cond
      ((null? expression) (remove-state-layer state))
      ((eq? (operator (car expression)) 'break) (break (remove-state-layer state)))
      ((eq? (operator (car expression)) 'continue) (continue (remove-state-layer state)))
      ((eq? (operator (car expression)) 'throw) (throw (remove-state-layer state)))
      (else (block-state (cdr expression) (update-state
                                           (car expression)
                                           state break continue return throw)
                         break continue return throw)))))


