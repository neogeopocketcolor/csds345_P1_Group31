#lang racket
(require "simpleParser.rkt")

(define comand caar)
(define statement car)
(define nextStatement cdr)
(define body caddr)
(define condition cadr)
(define statement1 caddr)
(define statement2 cadddr)


;returns stateList -- the state represented by a list
(define M-state
  (lambda (lis stateList)
    (cond
      [(null? lis) stateList]
      [(eq? (comand lis) '=) (M-state (nextStatement lis) (M-assign (statement lis) stateList))]
      [(eq? (comand lis) 'if) (M-state (nextStatement lis) (if (condition lis) (M-state (statement1 lis)) (M-state (statement2 lis))))]
      [(eq? (comand lis) 'return) (M-state (statement lis) stateList)]
      [else 'placeholder])))





;NEEDS TO TAKE STATELIST, NEEDS TO BE ABLE TO USE VARS
(define M-expression
  (lambda (lis)
    (if (math? (operator lis)) ;checks if comand is a mathematical expression
        (M-integer lis stateList) 
        (M-boolean lis stateList)))) ;expressions can only be mathematical or boolean, might be a source of bugs (not checking if it is a boolean expression)

;tests if val is a number or math operator
(define math?
  (lambda (val)
    (cond
      [(number? val) #t]
      [(eq? '+) #t]
      [(eq? '-) #t]
      [(eq? '/) #t]
      [(eq? '*) #t]
      [(eq? '%) #t]
      [else #f])))


(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)




;NEEDS TO TAKE STATELIST, NEEDS TO BE ABLE TO USE VARS
(define M-integer ;<op> <operand> <operand>
  (lambda (lis)
    (cond
      [(number? lis) lis]
      [(eq? (operator lis) '+) (+ (M-integer (leftoperand lis)) (M-integer (rightoperand lis)))]
      [(and (eq? (operator lis) '-) (null? (rightoperand lis))) (- 0 (M-integer (leftoperand lis)))]
      [(eq? (operator lis) '-) (- (M-integer (leftoperand lis)) (M-integer (rightoperand lis)))]
      [(eq? (operator lis) '*) (* (M-integer (leftoperand lis)) (M-integer (rightoperand lis)))]
      [(eq? (operator lis) '/) (quotient (M-integer (leftoperand lis)) (M-integer (rightoperand lis)))]
      [(eq? (operator lis) '%) (remainder (M-integer (leftoperand lis)) (M-integer (rightoperand lis)))])))


;NEEDS TO TAKE STATELIST, NEEDS TO BE ABLE TO USE VARS
(define M-boolean
  (lambda (lis)
    (cond
      [(eq? lis 'true) #t]
      [(eq? lis 'false) #f]
      [(eq? (operator lis) '&&) (and (M-boolean (leftoperand lis)) (M-boolean (rightoperand lis)))]
      [(eq? (operator lis) '||) (or (M-boolean (leftoperand lis)) (M-boolean (rightoperand lis)))]
      [(eq? (operator lis) '!)  (not (M-boolean leftoperand lis))]
      [(eq? (operator lis) '==) (eq? (M-expression (leftoperand lis)) (M-expression (rightoperand lis)))]
      [(eq? (operator lis) '>)  (> (M-expression (leftoperand lis)) (M-expression (rightoperand lis)))]
      [(eq? (operator lis) '<)  (< (M-expression (leftoperand lis)) (M-expression (rightoperand lis)))]
      [(eq? (operator lis) '<=) (<= (M-expression (leftoperand lis)) (M-expression (rightoperand lis)))]
      [(eq? (operator lis) '>=) (>= (M-expression (leftoperand lis)) (M-expression (rightoperand lis)))]
      [(eq? (operator lis) '!=) (not (eq? (M-expression (leftoperand lis)) (M-expression (rightoperand lis))))]
      [else 'M-booleanError])))
      
    
