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
(define beginBody cdar)
(define emptyReturn `())

(define initialState '(()))
(define initialNext (lambda (v) v))
(define initialBreak (lambda (v) v))
(define initialThrow (lambda (v) v))

(define variableDec car)
(define varValue cadr)
(define value cddr)

(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

(define frontState car)
(define followingStates cdr)
(define innerFollowingStates cadr)
(define innerState caar)

(define returnVal cadr)
(define returnify (lambda (v) (list 'return v)))

(define finallyShortcut
  (lambda (v) car (cadr (cdr (cdr (car v))))))
(define catchShortcut
  (lambda (v) car (cadr (cdr (car v)))))

(define pop cdr)
(define push (lambda (v) (cons '() v)))


;interpret command - required, parses the input file and executes the code.
(define interpret
  (lambda (filename)
    (M-state (parser filename) initialState initialNext initialBreak initialThrow)))

;M-state - updates the stateList based on the current command at the front of the list.
(define M-state
  (lambda (lis stateList next break throw) ;TODO: change all cases to take in / work with new parameters
    (cond
      [(null? lis) stateList]
      [(eq? (command lis) '=)      (M-assign (statement lis) stateList (lambda (s) (M-state (lambda s (nextStatement lis) s next break throw))))]
      [(eq? (command lis) 'var)    (M-declare (statement lis) stateList next) (lambda (s) (M-state (nextStatement lis) s next break throw))]
      [(eq? (command lis) 'if)     (if (M-boolean (condition lis) stateList)
                                       (M-state (statement1 lis) stateList (lambda (s) (M-state (nextStatement lis) s next break throw)) break throw)
                                       (if (not (null? (M-else lis)))
                                           (M-state (statement2 lis) stateList (lambda (s) (M-state (nextStatement lis) s next break throw)) break throw)
                                           (next stateList)))] 
      [(eq? (command lis) 'while)  (loop (condition lis) (body lis) stateList (M-state (nextStatement lis) stateList) (lambda (s) (M-state (nextStatement lis) stateList)) throw)]
      [(eq? (command lis) 'return) (M-return (statement lis) stateList)]
      [(eq? (command lis) 'begin) (M-state (beginBody lis) (push stateList) (lambda (s) (next (pop s))) (lambda (s) (next s)))] 
      [(eq? (command lis) 'try) (M-state (beginBody lis) (push stateList) (M-state (finallyShortcut lis) (lambda (s) M-state (nextStatement lis) s break throw)) ;next, go to finally
                                                                            (M-state (finallyShortcut lis) stateList (lambda (s) M-state (nextStatement lis) s break throw)) ;if broken, go to finally
                                                                              (M-state (catchShortcut lis) stateList ;if exception is thrown, go to catch
                                                                                       (lambda (s1) (M-state (finallyShortcut lis) s1 (lambda (s2) M-state (nextStatement lis) s2 break)));catch's next statement is finally
                                                                                       (lambda (s1) (M-state (finallyShortcut lis) s1 (lambda (s2) M-state (nextStatement lis) s2 break)))))] ;catch's break statement is finally
      [(eq? (command lis) 'catch) (M-state (beginBody lis) stateList (lambda (s) (next s)) (lambda (s) (next s)) throw)]
      [(eq? (command lis) 'throw) (throw stateList)]
      [(eq? (command lis) 'finally) (M-state (beginBody lis) stateList (lambda (s) (next (pop s))) (lambda (s) (next (pop s))) throw)] ;return popped state
      [(eq? (command lis) 'break) (break (pop stateList))]
      [(eq? (command lis) 'continue) (next stateList)] 
      [else (error 'Interpreter "Not a valid command")]))) 

;Helper function for loops
(define loop
  (lambda (condition body stateList next break)
    (if (M-boolean condition stateList)
        (M-state body stateList (lambda (s) (loop condition body s next break)) break)
        (next stateList))))


;M-declare - declares a variable, either with or without a binding to a value.
(define M-declare
  (lambda (lis stateList next)
    (if (null? (value lis))
        (AddBinding (varValue lis) (next stateList)) ;declare only
        (next (M-assign (cdr lis) (AddBinding (varValue lis) stateList) next))))) ;declare and assign
        
;M-assign - assigns a binding to a variable if the variable doesn't already have a value.
(define M-assign 
  (lambda (lis stateList next)
    (if (not (declared? (leftoperand lis) stateList))
        (error 'Interpreter "Variable not declared. :(")
        (next (ChangeBinding (leftoperand lis) (M-expression (rightoperand lis) stateList) stateList)))))
       
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
  (lambda (var bigStateList)
    (cond
      ((null? bigStateList) #f)
      ((declaredInside? var (car bigStateList)) #t)
      (else (declared? var (cdr bigStateList))))))

(define declaredInside?
  (lambda (var stateList)
    (cond
      ((null?  stateList) #f)
      ((equal? (variableDec stateList) var) #t)
      (else (declared? var (cdr stateList))))))

;AddBinding - takes a var name and the statelist, creates a new binding with given var.
(define AddBinding
  (lambda (var stateList)
    (if (declared? var (frontState stateList))
        (error 'Interpreter "Variable already declared.")
        (cons (list var 'null) (frontState stateList)))))


;CheckBinding - takes a var name and statelist, then returns the value of the variable. returns the first instance of said variable
(define CheckBinding
  (lambda (var bigStateList)
    (cond
      ((null? bigStateList) (error 'Interpreter "Variable has not been declared."))
      ((equal? (frontState (CheckBindingInside var (frontState bigStateList))) var) (innerFollowingStates (CheckBindingInside var (frontState (bigStateList)))))
      (else (CheckBinding var (followingStates bigStateList))))))

;takes a sub-stateList, and returns the binding of a corresponding variable if it exists. (var varValue)
(define CheckBindingInside
  (lambda (var stateList)
    (cond
      ((null? stateList) emptyReturn)
      ((equal? (innerState stateList) var) (frontState stateList))
      (else (CheckBinding var (followingStates stateList))))))


;ChangeBinding - takes a var name, value, and stateList, then returns the stateList with the new variable's value updated.
(define ChangeBinding
  (lambda (var newVal bigStateList)
    (cond
      [(null? bigStateList) (error `Interpreter "Variable has not been declared.")]
      [(declared? var (frontState bigStateList))(cons (ChangeBindingInside var newVal (frontState bigStateList)) (followingStates bigStateList))]
      [else (cons (frontState bigStateList) (ChangeBinding var newVal (followingStates bigStateList)))])))

(define ChangeBindingInside
  (lambda (var newVal stateList)
    (cond
      ((null? stateList) stateList)
      ((equal? (variableDec stateList) var) 
       (cons (list var newVal) (followingStates stateList)))
      (else
       (cons (frontState stateList) (ChangeBinding var newVal (followingStates stateList)))))))

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
      ((null? statement) (error 'Interpreter "M-return error - Null statement somehow"))
      ((number? (returnVal statement)) (returnVal statement))
      ((eq? #t (returnVal statement)) 'true)
      ((eq? #f (returnVal statement)) 'false)
      ((pair? (returnVal statement)) (M-return (returnify (M-expression (returnVal statement) stateList)) stateList)) ;if an expression, call m-expression
      ((declared? (returnVal statement) stateList) (M-return (returnify (CheckBinding (returnVal statement) stateList)) stateList)) ;check if statement is a declared variable, if so return the value.
      (else (error 'Interpreter "M-return error - Not accounted for")))))

;Test Statements
(interpret "testthis.txt")

;END

 
