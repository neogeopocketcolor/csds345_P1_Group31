#lang racket
(require "simpleParser.rkt")


#|

To Do List:

error checks (especially making sure variables are declared/initialized before use)
return


|#

;Abstractions
(define comand caar)
(define statement car)
(define nextStatement cdr)
(define body caddr)
(define condition cadr)
(define statement1 caddr)
(define M-else caddr)
(define statement2 (lambda (v) (cadr (cddr v))))

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
      [(eq? (comand lis) 'if)     (M-state (nextStatement lis) (if (M-boolean (condition lis) stateList)
                                                                   (M-state (statement1 lis))
                                                                   (if (not (null? (M-else lis)))
                                                                       (M-state (statement2 lis))
                                                                       stateList)))]
      [(eq? (comand lis) 'while)  (if (M-boolean (condition lis) stateList)
                                          (M-state lis (M-state (body lis) stateList))
                                          (M-state (nextStatement lis) stateList))]
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
      [else (ChangeBinding (leftoperand lis) (M-expression (rightoperand lis) stateList) stateList)])))
       
;Doesn't work for a variable
(define M-expression ;returns number if math, boolean if not
  (lambda (lis stateList)
    (cond
      [(declared? lis stateList) (if (math? (CheckBinding lis stateList))
                             (M-integer lis stateList)
                             (M-boolean lis stateList))]
      [(not (list? lis)) (if (math? lis)
                             (M-integer lis stateList)
                             (M-boolean lis stateList))]
      [(math? (operator lis)) (M-integer lis stateList)]
      [else (M-boolean lis stateList)]))) ;expressions can only be mathematical or boolean, might be a source of bugs (not checking if it is a boolean expression)

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
        (error 'Interpreter "Variable already declared.")
        (cons stateList (list (list var 'null)))))) ;abstract more? maybe?

;CheckBinding takes a var name and statelist, then returns the value of the variable
(define CheckBinding
  (lambda (var stateList)
    (cond
      ((null? stateList) (error 'Interpreter "Variable has not been declared."))
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


;Evaluates a mathematical expression
(define M-integer ;returns a number
  (lambda (lis stateList)
    (cond
      [(number? lis) lis]
      [(not (list? lis)) (CheckBinding lis stateList)]
      [(eq? (operator lis) '+) (+ (M-integer (leftoperand lis) stateList) (M-integer (rightoperand lis) stateList))]
      [(and (eq? (operator lis) '-) (null? (rightoperand lis))) (- 0 (M-integer (leftoperand lis)) stateList)]
      [(eq? (operator lis) '-) (- (M-integer (leftoperand lis) stateList) (M-integer (rightoperand lis) stateList))]
      [(eq? (operator lis) '*) (* (M-integer (leftoperand lis) stateList) (M-integer (rightoperand lis) stateList))]
      [(eq? (operator lis) '/) (quotient (M-integer (leftoperand lis) stateList) (M-integer (rightoperand lis) stateList))]
      [(eq? (operator lis) '%) (remainder (M-integer (leftoperand lis) stateList) (M-integer (rightoperand lis) stateList))]
      [else (error 'Interpreter "M-integer_Error")])))


;Evaluates a boolean expression
(define M-boolean ;returns #t or #f
  (lambda (lis stateList)
    (cond
      [(eq? lis 'true) #t]
      [(eq? lis 'false) #f]
      [(not (list? lis)) (M-boolean (CheckBinding lis stateList) stateList)]
      [(eq? (operator lis) '&&) (and (M-boolean (leftoperand lis) stateList) (M-boolean (rightoperand lis) stateList))]
      [(eq? (operator lis) '||) (or  (M-boolean (leftoperand lis) stateList) (M-boolean (rightoperand lis) stateList))]
      [(eq? (operator lis) '!)  (not (M-boolean (leftoperand lis) stateList))]
      [(eq? (operator lis) '==) (eq? (M-expression (leftoperand lis) stateList) (M-expression (rightoperand lis) stateList))]
      [(eq? (operator lis) '>)  (>   (M-expression (leftoperand lis) stateList) (M-expression (rightoperand lis) stateList))]
      [(eq? (operator lis) '<)  (<   (M-expression (leftoperand lis) stateList) (M-expression (rightoperand lis) stateList))]
      [(eq? (operator lis) '<=) (<=  (M-expression (leftoperand lis) stateList) (M-expression (rightoperand lis) stateList))]
      [(eq? (operator lis) '>=) (>=  (M-expression (leftoperand lis stateList)) (M-expression (rightoperand lis) stateList))]
      [(eq? (operator lis) '!=) (not (eq? (M-expression (leftoperand lis) stateList) (M-expression (rightoperand lis) stateList)))]
      [else (error 'Interpreter "M-boolean_Error")])))
      
    