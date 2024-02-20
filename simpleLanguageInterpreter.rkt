#lang racket
(require "simpleParser.rkt")


#|

To Do List:
add variable functionality to M-integer & M-boolean
error checks (especially making sure variables are declared/initialized before use)
CheckBinding
ChangeBinding
declared?
while


|#

;Abstractions
(define comand caar)
(define statement car)
(define nextStatement cdr)
(define body caddr)
(define condition cadr)
(define statement1 caddr)
(define M-else caddr)
(define statement2 (cadr (cddr)))

(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)


;returns stateList -- the state represented by a list
(define M-state ;returns stateList
  (lambda (lis stateList)
    (cond
      [(null? lis) stateList]
      [(eq? (comand lis) '=)      (M-state (nextStatement lis) (M-assign (statement lis) stateList))]
      [(eq? (comand lis) 'var)    (M-state (nextStatement lis) (M-declare (statement lis) stateList))]
      [(eq? (comand lis) 'if)     (M-state (nextStatement lis) (if (condition lis)
                                                                   (M-state (statement1 lis))
                                                                   (if (not (null? (M-else lis)))
                                                                       (M-state (statement2 lis))
                                                                       stateList)))]
      ;while
      [(eq? (comand lis) 'return) (M-state (statement lis) stateList)]
      [else 'placeholder]))) ;adjust this

(define M-declare ;returns updated stateList
  (lambda (lis stateList)
    (if (null? (rightoperand lis))
        (AddBinding (leftoperand lis) stateList) ;declare only
        (M-assign lis (AddBinding (leftoperand lis) stateList))))) ;declare and assign
      

(define M-assign ;returns updated stateList
  (lambda (lis stateList)
    (cond
      [(not (declared? (leftoperand lis) stateList) (error 'Interpreter "Variable not declared. :("))]
      [(eq? (CheckBinding (leftoperand lis) stateList) 'null) (error 'Interpreter "Variable not initialized.")]
      [else (ChangeBinding (leftoperand lis) (M-expression (rightOperand lis) stateList) stateList)])))
       
;NEEDS TO TAKE STATELIST, NEEDS TO BE ABLE TO USE VARS
(define M-expression ;returns number if math, boolean if not
  (lambda (lis)
    (if (math? (operator lis)) ;checks if comand is a mathematical expression
        (M-integer lis stateList) 
        (M-boolean lis stateList)))) ;expressions can only be mathematical or boolean, might be a source of bugs (not checking if it is a boolean expression)

;tests if val is a number or math operator,  returning #t if it is, #f otherwise
(define math?
  (lambda (val)
    (cond
      [(number? val) #t]
      [(eq? '+ val) #t]
      [(eq? '- val) #t]
      [(eq? '/ val) #t]
      [(eq? '* val) #t]
      [(eq? '% val) #t]
      [else #f])))

;declared? takes a var name and the stateList, returning #t if var name exists in statelist. ex: (declared? 'x ((x 3))) returns #t

(define declared?
  (lambda (var stateList)
    (cond
      ((null? stateList) #f)
      ((equal? (caar stateList) var) #t)
      (else (declared? var (cdr stateList))))))

;AddBinding takes a var name and the statelist, creates a new binding with given var 
(define AddBinding ;returns updated stateList
  (lambda (var stateList)
    (if (declared? var stateList)
        (error 'Interpreter "Variable already declared."
        (cons stateList (list (list var 'null))))))) ;abstract more? maybe?

;CheckBinding takes a var name and statelist, then returns the value of the variable
(define CheckBinding
  (lambda (var stateList)
    (cond
      ((null? stateList) #f)
      ((equal? (caar stateList) var) (cadar stateList))
      (else (CheckBinding var (cdr stateList))))))
    

;ChangeBinding takes a var name, value, and stateList, then returns the stateList with the new variable value,
;Probably needs the declared? variable check, + more abstraction
(define ChangeBinding
  (lambda (var newVal stateList)
    (cond
      ((null? stateList) stateList)
      ((equal? (caar stateList) var) 
       (cons (list var newVal) (cdr stateList)))
      (else
       (cons (car stateList) (ChangeBinding var newVal (cdr stateList)))))))


;NEEDS TO TAKE STATELIST, NEEDS TO BE ABLE TO USE VARS
(define M-integer ;returns a number
  (lambda (lis)
    (cond
      [(number? lis) lis]
      [(eq? (operator lis) '+) (+ (M-integer (leftoperand lis)) (M-integer (rightoperand lis)))]
      [(and (eq? (operator lis) '-) (null? (rightoperand lis))) (- 0 (M-integer (leftoperand lis)))]
      [(eq? (operator lis) '-) (- (M-integer (leftoperand lis)) (M-integer (rightoperand lis)))]
      [(eq? (operator lis) '*) (* (M-integer (leftoperand lis)) (M-integer (rightoperand lis)))]
      [(eq? (operator lis) '/) (quotient (M-integer (leftoperand lis)) (M-integer (rightoperand lis)))]
      [(eq? (operator lis) '%) (remainder (M-integer (leftoperand lis)) (M-integer (rightoperand lis)))]
      [else (error 'Interpreter "M-integer_Error")])))


;NEEDS TO TAKE STATELIST, NEEDS TO BE ABLE TO USE VARS
(define M-boolean ;returns #t or #f
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
      [else (error 'Interpreter "M-boolean_Error")])))
      
    