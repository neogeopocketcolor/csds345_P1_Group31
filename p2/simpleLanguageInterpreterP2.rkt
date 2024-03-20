#lang racket
(require "simpleParser.rkt")

#|

Alex Seidman - Avry Rechel - Shea Leech
ads206 - ajr250 - csl86
3/22/2024
CSDS 345
Project 2 - Simple Language Interpreter

|#

;Abstractions
(define command caar)
(define statement car)
(define nextStatement cdr)
(define body cddar)
(define condition cadar)
(define statement1 (lambda (v) (list (caddar v))))
(define M-else (lambda (v) (cdr (cddar v))))
(define statement2 (lambda (v) (list (cadr (cddar v)))))

(define variableDec caar)
(define varValue cadr)
(define value cddr)

(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

(define returnVal cadr)
(define returnify (lambda (v) (list 'return v)))
 
;interpret command - required, parses the input file and executes the code.
(define interpret
  (lambda (filename)
    (stateTreeStarter (parser filename) '()))) ;maybe abstract the '()?

;====NEW!====
;stateTreeStarter - extension of interpret, a (probably) botched use of call/cc to start the first state level
(define stateTreeStarter
  (lambda (lis stateList)
    (call/cc
     (lambda (return)
       (cond
         ((null? lis) stateList)
         (else (stateTreeStarter (cdr lis) (M-state lis stateList return))))))))

;M-state - updates the stateList based on the current command at the front of the list.
(define M-state
  (lambda (lis stateList return)
    ;====================
    ;HI LOOK HERE
    ;I threw in a temporary "return" into M-state
    ;That would be used in conjunction w/ the return made by stateTreeStarter.
    ;Again this is just a temporary idea to get something down, I legit have no clue where to go from here
    ;I want to die :33333
    ;====================
    (cond
      [(null? lis) stateList]
      [(eq? (command lis) '=)      (M-state (nextStatement lis) (M-assign (statement lis) stateList) return)]
      [(eq? (command lis) 'var)    (M-state (nextStatement lis) (M-declare (statement lis) stateList) return)]
      [(eq? (command lis) 'if)     (M-state (nextStatement lis) (if (M-boolean (condition lis) stateList)
                                                                   (M-state (statement1 lis) stateList)
                                                                   (if (not (null? (M-else lis)))
                                                                       (M-state (statement2 lis) stateList)
                                                                       stateList))
                                            return)]
      [(eq? (command lis) 'while)  (if (M-boolean (condition lis) stateList)
                                          (M-state lis (M-state (body lis) stateList return) return)
                                          (M-state (nextStatement lis) stateList return))]
      [(eq? (command lis) 'return) (M-return (statement lis) stateList)]
      ;Placeholders for the new states
      ;Begin - Maybe calls stateTreeStarter to make a new stateTree? In some way, this needs to call *something* that makes a new layer.
      [(eq? (command lis) 'begin) (error 'Interpreter "needs implementation")]
      ;Try - same thing here, this needs to do something very similar to Begin (no shit sherlock)
      [(eq? (command lis) 'try) (error 'Interpreter "needs implementation")]
      ;Catch - barely any ideas for this one. Maybe something where if M-state catches a "try", then it first gets the (car (cdr cdr)) to know what the Catch is
      ;And then from there a catch function defines how to throw the error. Speaking of,
      [(eq? (command lis) 'catch) (error 'Interpreter "needs implementation")]
      ;Throw - isn't this supposed to be a goto for throwing an *error*. Why do the examples have it as like. A pseudo-return or whatever. I wanna explode
      [(eq? (command lis) 'throw) (error 'Interpreter "needs implementation")]
      [(eq? (command lis) 'finally) (error 'Interpreter "needs implementation")]
      [(eq? (command lis) 'break) (error 'Interpreter "needs implementation")]
      [(eq? (command lis) 'continue) (error 'Interpreter "needs implementation")] 
      [else (error 'Interpreter "Not a valid command")]))) 

;M-declare - declares a variable, either with or without a binding to a value.
(define M-declare
  (lambda (lis stateList)
    (if (null? (value lis))
        (AddBinding (varValue lis) stateList) ;declare only
        (M-assign lis (AddBinding (varValue lis) stateList))))) ;declare and assign
        
;M-assign - assigns a binding to a variable if the variable doesn't already have a value.
(define M-assign 
  (lambda (lis stateList)
    (if (not (declared? (leftoperand lis) stateList))
        (error 'Interpreter "Variable not declared. :(")
        (ChangeBinding (leftoperand lis) (M-expression (rightoperand lis) stateList) stateList))))
       
;M-expression - checks if an operation needs to return a number (math equation) or a boolean (t/f).
(define M-expression
  (lambda (lis stateList)
    (cond
      [(not (list? lis)) (if (math? lis)
                             (M-integer lis stateList)
                             (M-boolean lis stateList))]
      [(declared? lis stateList) (M-expression (CheckBinding lis stateList) stateList)]
      [(math? (operator lis)) (M-integer lis stateList)]
      [else (M-boolean lis stateList)])))

;math? - tests if val is a number or math operator, returning #t if it is, #f otherwise.
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

;declared? - takes a var name and the stateList, returning #t if var name exists in statelist. ex: (declared? 'x ((x 3))) returns #t.
(define declared?
  (lambda (var stateList)
    (cond
      ((null?  stateList) #f)
      ((equal? (variableDec stateList) var) #t)
      (else (declared? var (cdr stateList))))))

;AddBinding - takes a var name and the statelist, creates a new binding with given var.
(define AddBinding
  (lambda (var stateList)
    (if (declared? var stateList)
        (error 'Interpreter "Variable already declared.")
        (cons (list var 'null) stateList))))


;CheckBinding - takes a var name and statelist, then returns the value of the variable.
(define CheckBinding
  (lambda (var stateList)
    (cond
      ((null? stateList) (error 'Interpreter "Variable has not been declared."))
      ((equal? (caar stateList) var) (cadar stateList))
      (else (CheckBinding var (cdr stateList))))))
    

;ChangeBinding - takes a var name, value, and stateList, then returns the stateList with the new variable's value updated.
(define ChangeBinding
  (lambda (var newVal stateList)
    (cond
      ((null? stateList) stateList)
      ((equal? (variableDec stateList) var) 
       (cons (list var newVal) (cdr stateList)))
      (else
       (cons (car stateList) (ChangeBinding var newVal (cdr stateList)))))))

;M-integer - checks what kind of operation needs to be performed, returns an integer.
(define M-integer
  (lambda (lis stateList)
    (cond
      [(number? lis) lis]
      [(not (list? lis)) (CheckBinding lis stateList)]
      [(eq? (operator lis) '+) (+ (M-integer (leftoperand lis) stateList) (M-integer (rightoperand lis) stateList))]
      [(and (eq? (operator lis) '-) (null? (value lis))) (- 0 (M-integer (leftoperand lis) stateList))]
      [(eq? (operator lis) '-) (- (M-integer (leftoperand lis) stateList) (M-integer (rightoperand lis) stateList))]
      [(eq? (operator lis) '*) (* (M-integer (leftoperand lis) stateList) (M-integer (rightoperand lis) stateList))]
      [(eq? (operator lis) '/) (quotient (M-integer (leftoperand lis) stateList) (M-integer (rightoperand lis) stateList))]
      [(eq? (operator lis) '%) (remainder (M-integer (leftoperand lis) stateList) (M-integer (rightoperand lis) stateList))]
      [else (error 'Interpreter "M-integer_Error")])))

;M-boolean - checks what kind of comparison must be made, returns either #t or #f.
(define M-boolean
  (lambda (lis stateList)
    (cond
      [(eq? lis 'true) #t]
      [(eq? lis 'false) #f]
      [(not (list? lis)) (CheckBinding lis stateList)]
      [(eq? (operator lis) '&&) (and (M-boolean (leftoperand lis) stateList) (M-boolean (rightoperand lis) stateList))]
      [(eq? (operator lis) '||) (or (M-boolean (leftoperand lis) stateList) (M-boolean (rightoperand lis) stateList))]
      [(eq? (operator lis) '!)  (not (M-boolean leftoperand lis) stateList)]
      [(eq? (operator lis) '==) (eq? (M-expression (leftoperand lis) stateList) (M-expression (rightoperand lis) stateList))]
      [(eq? (operator lis) '>)  (> (M-expression (leftoperand lis) stateList) (M-expression (rightoperand lis) stateList))]
      [(eq? (operator lis) '<)  (< (M-expression (leftoperand lis) stateList) (M-expression (rightoperand lis) stateList))]
      [(eq? (operator lis) '<=) (<= (M-expression (leftoperand lis) stateList) (M-expression (rightoperand lis) stateList))]
      [(eq? (operator lis) '>=) (>= (M-expression (leftoperand lis) stateList) (M-expression (rightoperand lis) stateList))]
      [(eq? (operator lis) '!=) (not (eq? (M-expression (leftoperand lis) stateList) (M-expression (rightoperand lis) stateList)))]
      [else (error 'Interpreter "M-boolean_Error")])))
      

;M-return - prints out the requested return value. Makes sure that #t/#f becomes 'true and 'false as well.
(define M-return
  (lambda (statement stateList)
    (cond
      ((null? statement) (error 'Interpreter "dude what"))
      ((number? (returnVal statement)) (returnVal statement))
      ((eq? #t (returnVal statement)) 'true)
      ((eq? #f (returnVal statement)) 'false)
      ((pair? (returnVal statement)) (M-return (returnify (M-expression (returnVal statement) stateList)) stateList)) ;if an expression, call m-expression
      ((declared? (returnVal statement) stateList) (M-return (returnify (CheckBinding (returnVal statement) stateList)) stateList)) ;check if statement is a declared variable, if so return the value.
      (else (error 'Interpreter "M-return error")))))

;END

 