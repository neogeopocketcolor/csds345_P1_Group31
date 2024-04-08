#lang racket
(require "functionParser.rkt")

#|

Alex Seidman - Avry Rechel
ads206 - ajr250
3/26/2024
CSDS 345
Project 2 - Simple Language Interpreter

|#

;;
;;;Abstractions
;;

;misc
(define command caar)
(define statement car)
(define nextStatement cdr)
(define body cddar)
(define condition cadar)
(define statement1 (lambda (v) (list (caddar v))))
(define M-else (lambda (v) (cdr (cddar v))))
(define statement2 (lambda (v) (list (cadr (cddar v)))))
(define beginBody cdar)
(define emptyReturn '(()))
(define lisBeginning car)

;initial values
(define initialState '(()))
(define initialNext (lambda (v) v))
(define initialBreak (lambda (v) (error 'Interpreter "'break' command executed in main.")))
(define initialThrow (lambda (v) (error 'Interpreter "'throw' must be used within a 'try'")))

;variables
(define variableDec caar)
(define varValue cadr)
(define value cddr)

;operations
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

;state list related
(define frontState car)
(define followingStates cdr)
(define innerFollowingStates cadr)
(define innerState caar)

;returns
(define returnVal cadr)
(define returnify (lambda (v) (list 'return v)))

;finally/catch
(define finallyShortcut
  (lambda (v) (cdr (cdr (cdr (car v))))))
(define finallyPoint car)
(define finallyPointAlt cadr)
(define catchShortcut
  (lambda (v) (list (cadr (cdr (car v))))))

;state popping and pushing
(define pop cdr)
(define push (lambda (v) (cons '() v)))

;;
;;;Proper Functions
;;

;interpret command - required, parses the input file and executes the code.
(define interpret
  (lambda (filename)
    (call/cc
     (lambda (initialReturn)
       (M-state (parser filename) initialState initialNext initialBreak initialThrow initialReturn)))))

;M-state-first - First run of M-state that stores functions and variables, allows variables to be set, and that's it. Throw everything else.
(define M-state-first
  (lambda (lis stateList next break throw return)
    (cond
      [(null? lis) stateList]
      [(eq? (command lis) '=)        (M-assign (statement lis) stateList (lambda (s) (next (M-state (nextStatement lis) s next break throw return))))]
      [(eq? (command lis) 'var)      (M-declare (statement lis) stateList (lambda (s) (next (M-state (nextStatement lis) s next break throw return))))]
      [(eq? (command lis) 'function) "how we will evaluate functions"]
      [(eq? (command lis) 'funcall)  "how we call functions"]
      [else                          (error 'Interpreter "Unallowed operation outside of functions")]
      )))
      
;M-state - updates the stateList based on the current command at the front of the list.
(define M-state
  (lambda (lis stateList next break throw return)
    (cond
      [(null? lis) stateList]
      [(eq? (command lis) '=)        (M-assign (statement lis) stateList (lambda (s) (next (M-state (nextStatement lis) s next break throw return))))]
      [(eq? (command lis) 'var)      (M-declare (statement lis) stateList (lambda (s) (next (M-state (nextStatement lis) s next break throw return))))]
      [(eq? (command lis) 'if)       (if (M-boolean (condition lis) stateList)
                                         (M-state (statement1 lis) stateList (lambda (s) (next (M-state (nextStatement lis) s next break throw return)) break throw return) break throw return)
                                         (if (not (null? (M-else lis)))
                                             (M-state (statement2 lis) stateList (lambda (s) (next (M-state (nextStatement lis) s next break throw return))) break throw return)
                                             (next (M-state (nextStatement lis) stateList next break throw return))))] 
      [(eq? (command lis) 'while)    (loop (condition lis) (body lis) stateList (lambda (s) (next (M-state (nextStatement lis) s next break throw return)))
                                           (lambda (s) (break (M-state (nextStatement lis) s next break throw return))) throw return)]
      [(eq? (command lis) 'return)   (return (M-return (statement lis) stateList))]
      [(eq? (command lis) 'begin)    (M-state (beginBody lis) (push stateList) (lambda (s) (next (M-state (nextStatement lis) (pop s) next break throw return)))
                                           (lambda (s) (call/cc (lambda k (break (M-state (nextStatement lis) (pop s) next k throw return))))) throw return)] 
      [(eq? (command lis) 'try)      (M-state (lisBeginning (beginBody lis)) (push stateList) (lambda (s1) (if (null? (finallyPoint (finallyShortcut lis))) (next (M-state (nextStatement lis) (pop s1) next break throw return)) ;if there's no finally, next go to nextStatement
                                                                                                      (next (M-state (finallyShortcut lis) s1 (lambda (s) (M-state (nextStatement lis) s next break throw return)) break throw return)))) ;next, go to finally
                                              (lambda (s1) (M-state (finallyShortcut lis) s1 (lambda (s) (next (M-state (nextStatement lis) s next break throw return))) break throw return)) ;if broken, go to finally
                                              (lambda (e s) (M-state (catchShortcut lis) (ChangeBinding (innerState (beginBody (catchShortcut lis))) e (AddBinding (innerState (beginBody (catchShortcut lis))) s)) ;if exception is thrown, go to catch
                                                                     (lambda (s1) (if (null? (finallyPoint (finallyShortcut lis))) (M-state (nextStatement lis) (pop s1) next break throw return)
                                                                                      (M-state (finallyShortcut lis) s1 (lambda (s2) (M-state (nextStatement lis) s2 next break throw return)) break throw return)));catch's next statement is finally
                                                                     break throw return)) return)] ;catch's break statement is finally
      [(eq? (command lis) 'catch)    (M-state (finallyPointAlt (beginBody lis)) stateList (lambda (s) (next s)) (lambda (s) (next s)) throw return)]
      [(eq? (command lis) 'throw)    (throw (cadar lis) stateList)]
      [(eq? (command lis) 'finally)  (M-state (lisBeginning (beginBody lis)) stateList
                                              (lambda (s) (next (pop s)))
                                              (lambda (s) (next (pop s))) throw return)] ;return popped state
      [(eq? (command lis) 'break)    (break stateList)]
      [(eq? (command lis) 'continue) (next stateList)]
      [(eq? (command lis) 'function) "how we will evaluate functions"]
      [(eq? (command lis) 'funcall) "how we call functions"]
      [else                          (error 'Interpreter "Not a valid command")])))

;
;; Function-Related Functions
;




;
;; General Use
;

;Helper function for loops
(define loop
  (lambda (condition body stateList next break throw return)
    (if (M-boolean condition stateList)
        (M-state body stateList
                 (lambda (s) (loop condition body s next break throw return))
                 break throw return)
        (next stateList))))

;M-declare - declares a variable, either with or without a binding to a value.
(define M-declare
  (lambda (lis stateList next)
    (if (null? (value lis))
        (next (AddBinding (varValue lis) stateList)) ;declare only
        (M-assign lis (AddBinding (varValue lis) stateList) next)))) ;declare and assign
        
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
                                 (M-integer lis stateList)(M-boolean lis stateList))]
      [(declared? lis stateList) (M-expression (CheckBinding lis stateList) stateList)]
      [(math? (operator lis))    (M-integer lis stateList)]
      [else                      (M-boolean lis stateList)])))

;math? - tests if val is a number or math operator, returning #t if it is, #f otherwise.
(define math?
  (lambda (val)
    (cond
      [(number? val) #t]
      [(eq? '+ val)  #t]
      [(eq? '- val)  #t]
      [(eq? '/ val)  #t]
      [(eq? '* val)  #t]
      [(eq? '% val)  #t]
      [else          #f])))

;
;;Variable Declare Functions
;

;declared? - takes a var name and the stateList, returning #t if var name exists in statelist. ex: (declared? 'x ((x 3))) returns #t.
(define declared?
  (lambda (var bigStateList)
    (cond
      ((null? bigStateList)                            #f)
      ((declaredInside? var (frontState bigStateList)) #t)
      (else                                            (declared? var (followingStates bigStateList))))))

;delcaredInside? - helper for declared? that dives into deeper states.
(define declaredInside?
  (lambda (var stateList)
    (cond
      ((null?  stateList)                  #f)
      ((equal?(variableDec stateList) var) #t)
      (else                                (declaredInside? var (followingStates stateList))))))

;AddBinding - takes a var name and the statelist, creates a new binding with given var.
(define AddBinding
  (lambda (var stateList)
    (if (declared? var stateList)
        (error 'Interpreter "Variable already declared.")
        (cons (cons (list var 'null) (frontState stateList)) (followingStates stateList)))))

;CheckBinding - takes a var name and statelist, then returns the value of the variable. returns the first instance of said variable
(define CheckBinding
  (lambda (var bigStateList)
    (cond
      ((null? bigStateList)                                                         (error 'Interpreter "Variable has not been declared."))
      ((equal? (frontState (CheckBindingInside var (frontState bigStateList))) var) (innerFollowingStates (CheckBindingInside var (frontState bigStateList))))
      (else                                                                         (CheckBinding var (followingStates bigStateList))))))

;takes a sub-stateList, and returns the binding of a corresponding variable if it exists. (var varValue)
(define CheckBindingInside
  (lambda (var stateList)
    (cond
      ((null? stateList)                     emptyReturn)
      ((equal? (variableDec stateList) var) (frontState stateList))
      (else                                 (CheckBindingInside var (followingStates stateList))))))


;ChangeBinding - takes a var name, value, and stateList, then returns the stateList with the new variable's value updated.
(define ChangeBinding
  (lambda (var newVal bigStateList)
    (cond
      [(null? bigStateList)                            (error `Interpreter "Variable has not been declared.")]
      [(declaredInside? var (frontState bigStateList)) (cons (ChangeBindingInside var newVal (frontState bigStateList)) (followingStates bigStateList))]
      [else                                            (cons (frontState bigStateList) (ChangeBinding var newVal (followingStates bigStateList)))])))

;ChangeBindingInside - helper for ChangeBinding for deeper states.
(define ChangeBindingInside
  (lambda (var newVal stateList)
    (cond
      ((null? stateList)                     stateList)
      ((equal? (variableDec stateList) var) (cons (list var newVal) (followingStates stateList)))
      (else                                 (cons (frontState stateList) (ChangeBindingInside var newVal (followingStates stateList)))))))

;
;;Function Declare Functions
;;;THE BIG SHIT WE NEED 2 DO I THINK ??
;;Function Declare Functions
;

;declaredFunction? - takes a var name and the stateList, returning #t if var name exists in statelist. ex: (declared? 'x ((x 3))) returns #t.
(define declaredFunction?
  (lambda (var bigStateList)
    (cond
      ((null? bigStateList)                            #f)
      ((declaredFunctionInside? var (frontState bigStateList)) #t)
      (else                                            (declared? var (followingStates bigStateList))))))

;declaredFunctionInside? - helper for declared? that dives into deeper states.
(define declaredFunctionInside?
  (lambda (var stateList)
    (cond
      ((null?  stateList)                  #f)
      ((equal?(variableDec stateList) var) #t)
      (else                                (declaredFunctionInside? var (followingStates stateList))))))

;AddFunctionBinding - takes a var name and the statelist, creates a new binding with given var.
(define AddFunctionBinding
  (lambda (var stateList)
    (if (declared? var stateList)
        (error 'Interpreter "Variable already declared.")
        (cons (cons (list var 'null) (frontState stateList)) (followingStates stateList)))))

;CheckFunctionBinding - takes a var name and statelist, then returns the value of the variable. returns the first instance of said variable
(define CheckFunctionBinding
  (lambda (var bigStateList)
    (cond
      ((null? bigStateList)                                                         (error 'Interpreter "Variable has not been declared."))
      ((equal? (frontState (CheckFunctionBindingInside var (frontState bigStateList))) var) (innerFollowingStates (CheckFunctionBindingInside var (frontState bigStateList))))
      (else                                                                         (CheckFunctionBinding var (followingStates bigStateList))))))

;CheckFunctionBindingInside - takes a sub-stateList, and returns the binding of a corresponding variable if it exists. (var varValue)
(define CheckFunctionBindingInside
  (lambda (var stateList)
    (cond
      ((null? stateList)                     emptyReturn)
      ((equal? (variableDec stateList) var) (frontState stateList))
      (else                                 (CheckFunctionBindingInside var (followingStates stateList))))))


;ChangeFunctionBinding - takes a var name, value, and stateList, then returns the stateList with the new variable's value updated.
(define ChangeFunctionBinding
  (lambda (var newVal bigStateList)
    (cond
      [(null? bigStateList)                            (error `Interpreter "Variable has not been declared.")]
      [(declaredInside? var (frontState bigStateList)) (cons (ChangeFunctionBindingInside var newVal (frontState bigStateList)) (followingStates bigStateList))]
      [else                                            (cons (frontState bigStateList) (ChangeFunctionBinding var newVal (followingStates bigStateList)))])))

;ChangeFunctionBindingInside - helper for ChangeBinding for deeper states.
(define ChangeFunctionBindingInside
  (lambda (var newVal stateList)
    (cond
      ((null? stateList)                     stateList)
      ((equal? (variableDec stateList) var) (cons (list var newVal) (followingStates stateList)))
      (else                                 (cons (frontState stateList) (ChangeFunctionBindingInside var newVal (followingStates stateList)))))))

;
;; M-state related checks
;

;M-integer - checks what kind of operation needs to be performed, returns an integer.
(define M-integer
  (lambda (lis stateList)
    (cond
      [(number? lis)                 lis]
      [(not (list? lis))            (CheckBinding lis stateList)]
      [(eq? (operator lis) '+)      (+ (M-integer (leftoperand lis) stateList) (M-integer (rightoperand lis) stateList))]
      [(and (eq? (operator lis) '-) (null? (value lis))) (- 0 (M-integer (leftoperand lis) stateList))]
      [(eq? (operator lis) '-)      (- (M-integer (leftoperand lis) stateList) (M-integer (rightoperand lis) stateList))]
      [(eq? (operator lis) '*)      (* (M-integer (leftoperand lis) stateList) (M-integer (rightoperand lis) stateList))]
      [(eq? (operator lis) '/)      (quotient (M-integer (leftoperand lis) stateList) (M-integer (rightoperand lis) stateList))]
      [(eq? (operator lis) '%)      (remainder (M-integer (leftoperand lis) stateList) (M-integer (rightoperand lis) stateList))]
      [else                         (error 'Interpreter "M-integer_Error")])))

;M-boolean - checks what kind of comparison must be made, returns either #t or #f.
(define M-boolean
  (lambda (lis stateList)
    (cond
      [(eq? lis 'true)          #t]
      [(eq? lis 'false)         #f]
      [(not (list? lis))        (CheckBinding lis stateList)]
      [(eq? (operator lis) '&&) (and (M-boolean (leftoperand lis) stateList) (M-boolean (rightoperand lis) stateList))]
      [(eq? (operator lis) '||) (or (M-boolean (leftoperand lis) stateList) (M-boolean (rightoperand lis) stateList))]
      [(eq? (operator lis) '!)  (not (M-boolean (leftoperand lis) stateList))]
      [(eq? (operator lis) '==) (eq? (M-expression (leftoperand lis) stateList) (M-expression (rightoperand lis) stateList))]
      [(eq? (operator lis) '>)  (> (M-expression (leftoperand lis) stateList) (M-expression (rightoperand lis) stateList))]
      [(eq? (operator lis) '<)  (< (M-expression (leftoperand lis) stateList) (M-expression (rightoperand lis) stateList))]
      [(eq? (operator lis) '<=) (<= (M-expression (leftoperand lis) stateList) (M-expression (rightoperand lis) stateList))]
      [(eq? (operator lis) '>=) (>= (M-expression (leftoperand lis) stateList) (M-expression (rightoperand lis) stateList))]
      [(eq? (operator lis) '!=) (not (eq? (M-expression (leftoperand lis) stateList) (M-expression (rightoperand lis) stateList)))]
      [else                     (error 'Interpreter "M-boolean_Error")])))
      

;M-return - prints out the requested return value. Makes sure that #t/#f becomes 'true and 'false as well.
(define M-return
  (lambda (statement stateList)
    (cond
      ((null? statement)                           (error 'Interpreter "M-return error - Null statement somehow"))
      ((number? (returnVal statement))             (returnVal statement))
      ((eq? #t (returnVal statement))              'true)
      ((eq? #f (returnVal statement))              'false)
      ((pair? (returnVal statement))               (M-return (returnify (M-expression (returnVal statement) stateList)) stateList)) ;if an expression, call m-expression
      ((declared? (returnVal statement) stateList) (M-return (returnify (CheckBinding (returnVal statement) stateList)) stateList)) ;check if statement is a declared variable, if so return the value.
      (else                                        (error 'Interpreter "M-return error - Not accounted for")))))

;END

(parser "testthis.txt")
(interpret "testthis.txt")
 
