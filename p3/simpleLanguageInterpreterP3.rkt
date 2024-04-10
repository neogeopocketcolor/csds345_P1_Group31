#lang racket
(require "functionParser.rkt")

#|

Alex Seidman - Avry Rechel
ads206 - ajr250
4/10/2024
CSDS 345
Project 3 - Imperitive Language Interpreter

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
(define initialFunc '(()))
(define initialNext (lambda (v1 v2) v1))
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

;functions
(define paramList car)
(define commandList cadr)

;;
;;;Proper Functions
;;

;interpret command - required, parses the input file and executes the code.
(define interpret
  (lambda (filename)
    (call/cc
     (lambda (initialReturn)
       (M-state (parser filename) initialState initialFunc (lambda (s f) (initialReturn (M-funcall '(funcall main ()) (push s) (push f) initialNext))) initialBreak initialThrow initialReturn)))))

#|
- Assumedly we’d want to go through the parsed code once first to store global variables.
    - Global variables can be set by functions declared before they’re called (Test 5)
    - Otherwise it’s *just* variable and function declaring. Parser throws error if anything else is tried.
    - Maybe have an M-state that goes thru
        - 1. Right when the (interpret) command is called
        - 2. Every time a new function is called
    - and stores the functions of an env before doing anything else?
|#
  
;M-state - updates the stateList based on the current command at the front of the list.
(define M-state
  (lambda (lis stateList funcList next break throw return)
    (cond
      [(null? lis) (next stateList funcList)]
      [(eq? (command lis) '=)        (M-assign (statement lis) stateList funcList (lambda (s f) (next (M-state (nextStatement lis) s f next break throw return) f)))]
      [(eq? (command lis) 'var)      (M-declare (statement lis) stateList funcList (lambda (s f) (next (M-state (nextStatement lis) s f next break throw return) f)))]
      
      [(eq? (command lis) 'if)       (if (M-boolean (condition lis) stateList funcList next)
                                         (M-state (statement1 lis) stateList funcList (lambda (s f) (next (M-state (nextStatement lis) s f next break throw return) f) break throw return) break throw return)
                                         (if (not (null? (M-else lis)))
                                             (M-state (statement2 lis) stateList funcList (lambda (s f) (next (M-state (nextStatement lis) s f next break throw return) f)) break throw return)
                                             (next (M-state (nextStatement lis) stateList funcList next break throw return) funcList)))]
      
      [(eq? (command lis) 'while)    (loop (condition lis) (body lis) stateList funcList (lambda (s f) (next (M-state (nextStatement lis) s funcList next break throw return) funcList))
                                           (lambda (s f) (break (M-state (nextStatement lis) s funcList next break throw return))) throw return)]
      [(eq? (command lis) 'return)   (return (M-return (statement lis) stateList funcList next))]
      
      [(eq? (command lis) 'begin)    (M-state (beginBody lis) (push stateList) (push funcList) (lambda (s f) (next (M-state (nextStatement lis) (pop s) (pop funcList) next break throw return))) ; push/pop funclist?
                                           (lambda (s f) (call/cc (lambda k (break (M-state (nextStatement lis) (pop s) (pop funcList) next k throw return))))) throw return)]
      
      [(eq? (command lis) 'try)      (M-state (lisBeginning (beginBody lis)) (push stateList) (push funcList) (lambda (s1) (if (null? (finallyPoint (finallyShortcut lis))) (next (M-state (nextStatement lis) (pop s1) funcList next break throw return))
                                                                                                      ;CHECK OUT THIS COMMENT YAYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY ^^^^^^^^ funcList remains? or pop this too
                                                                                                      (next (M-state (finallyShortcut lis) s1 funcList (lambda (s) (M-state (nextStatement lis) s funcList next break throw return)) break throw return)))) ;next, go to finally
                                              (lambda (s1) (M-state (finallyShortcut lis) s1 funcList (lambda (s) (next (M-state (nextStatement lis) s funcList next break throw return))) break throw return)) ;if broken, go to finally
                                              (lambda (e s f) (M-state (catchShortcut lis) (ChangeBinding (innerState (beginBody (catchShortcut lis))) e (AddBinding (innerState (beginBody (catchShortcut lis))) s funcList) funcList) ;if exception is thrown, go to catch
                                                                     (lambda (s1) (if (null? (finallyPoint (finallyShortcut lis))) (M-state (nextStatement lis) (pop s1) funcList next break throw return)
                                                                                      ;; THIS ONE TOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO ^^^^^^^^ pop this?
                                                                                      (M-state (finallyShortcut lis) s1 funcList (lambda (s2) (M-state (nextStatement lis) s2 funcList next break throw return)) break throw return)));catch's next statement is finally
                                                                     break throw return)) return)] ;catch's break statement is finally
      
      [(eq? (command lis) 'catch)    (M-state (finallyPointAlt (beginBody lis)) stateList funcList (lambda (s) (next s)) (lambda (s) (next s)) throw return)]
      [(eq? (command lis) 'throw)    (throw (cadar lis) stateList)]
      [(eq? (command lis) 'finally)  (M-state (lisBeginning (beginBody lis)) stateList funcList
                                              (lambda (s) (next (pop s)))
                                              (lambda (s) (next (pop s))) throw return)] ;return popped state
      
      [(eq? (command lis) 'break)    (break stateList)]
      [(eq? (command lis) 'continue) (next stateList)]
      
      [(eq? (command lis) 'function) (M-declareFunction (statement lis) stateList funcList (lambda (s f) (next (M-state (nextStatement lis) stateList f next break throw return) funcList)))]
      [(eq? (command lis) 'funcall)  (M-funcall (statement lis) (push stateList) (push funcList) (lambda (s) (next (M-state (nextStatement lis) (pop s) (pop funcList) next break throw return))))]
      [else                          (error 'Interpreter "Not a valid command")])))


;
;; General Use
;

;Helper function for loops
(define loop
  (lambda (condition body stateList funcList next break throw return)
    (if (M-boolean condition stateList funcList next)
        (M-state body stateList funcList
                 (lambda (s f) (loop condition body s funcList next break throw return))
                 break throw return)
        (next stateList funcList))))

;M-declare - declares a variable, either with or without a binding to a value.
(define M-declare
  (lambda (lis stateList funcList next)
    (if (null? (value lis))
        (next (AddBinding (varValue lis) stateList funcList)) ;declare only
        (M-assign lis (AddBinding (varValue lis) stateList) funcList next)))) ;declare and assign

        
;M-assign - assigns a binding to a variable if the variable doesn't already have a value.
(define M-assign 
  (lambda (lis stateList funcList next)
    (if (not (declared? (leftoperand lis) stateList))
        (error 'Interpreter "Variable not declared. :(")
        (next (ChangeBinding (leftoperand lis) (M-expression (rightoperand lis) stateList funcList next) stateList funcList) funcList))))

;M-declareFunction - declares a function, binding the function's name, (formal parameters), and (comamands), into one readable lis.
(define M-declareFunction
  (lambda (lis stateList funcList next)
    (next stateList (AddFunctionBinding (cdr lis) funcList))))

;M-funcall - handles the calling of a function. Finds if the function's name exists in stateList, and if it does
#|
    - Calls are translated as such:
        - name(param1 param2) <— java-esque call
        - (funcall name param1 param2) <— parser’s storage
    - So suppose functions are stored like
        - { [ (name) (param1 param2) (function’s body) ] }
        - {} being the list the current env’s functions are stored
        - [] being the list that stores a single function
    - So when funcall occurs,
        - 1. Check for the name (car of the list) in every current env’s function list
        - 2. If it exists, somehow step into the env and declare the parameters
            -  Take param1 from the storage, and declare that as a new variable with the value of the input value (cadr of funcall)
            - If lengths of param lists don’t match, throw error
        - 3. Call M-state on the first command of the list and just go from there as you would w/ a try/catch.
|#
(define M-funcall
  (lambda (lis stateList funcList next)
    (call/cc
     (lambda (initialReturn)
       (next (M-state (commandList (CheckFunctionBinding (leftoperand lis) funcList)) (parametize (paramList (CheckFunctionBinding (leftoperand lis) funcList)) (rightoperand lis) (push stateList) (push funcList) next) funcList initialNext initialBreak initialThrow initialReturn))))))
      ; ^^^ changed this to have push statelist/funclist as formal params can appear as declared variables in the same env. May fuck stuff up, works currently.

(define parametize
  (lambda (formal actual stateList funcList next)
    (if (null? formal)
        stateList
        (parametize (cdr formal) (parametizeCheckCdr actual) (M-declare (cons 'var (list (car formal) (M-expression (parametizeCheckCar actual) stateList funcList next))) stateList funcList next) funcList next))))

(define parametizeCheckCdr
  (lambda (val)
    (if (list? val)
        (cdr val)
        val)))

(define parametizeCheckCar
  (lambda (val)
    (if (and (list? val) (number? (car val)))
        (car val)
        val)))


;M-expression - checks if an operation needs to return a number (math equation) or a boolean (t/f).
(define M-expression
  (lambda (lis stateList funcList next)
    (cond
      [(not (list? lis)) (if (math? lis)
                                     (M-integer lis stateList funcList)
                                     (M-boolean lis stateList funcList next))]
      [(declared? lis stateList)     (M-expression (CheckBinding lis stateList funcList) stateList funcList next)]
      [(math? (operator lis))        (M-integer lis stateList funcList)]
      [(eq? (operator lis) 'funcall) (M-funcall lis stateList funcList (lambda (v f) v))] 
      [else                          (M-boolean lis stateList funcList next)])))

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
        ;
        ;
        ;
        ;
        ;
        ;
        ;
        ;
        ;
        (error 'Interpreter "Variable already declared.")
        (cons (cons (list var 'null) (frontState stateList)) (followingStates stateList)))))

;CheckBinding - takes a var name and statelist, then returns the value of the variable. returns the first instance of said variable
(define CheckBinding
  (lambda (var bigStateList)
    (cond
      ((null? bigStateList)                                                         (error 'Interpreter "Variable has not been declared."))
      ((equal? (frontState (CheckBindingInside var (frontState bigStateList))) var) (innerFollowingStates (CheckBindingInside var (frontState bigStateList))))
      (else                                                                         (CheckBinding var (followingStates bigStateList))))))

;CheckBindingInside - takes a sub-stateList, and returns the binding of a corresponding variable if it exists. (var varValue)
(define CheckBindingInside
  (lambda (var stateList)
    (cond
      ((null? stateList)                     emptyReturn)
      ((equal? (variableDec stateList) var) (frontState stateList))
      (else                                 (CheckBindingInside var (followingStates stateList))))))


;ChangeBinding - takes a var name, value, and stateList, then returns the stateList with the new variable's value updated.
(define ChangeBinding
  (lambda (var newVal bigStateList funcList)
    (cond
      [(null? bigStateList)                            (error `Interpreter "Variable has not been declared.")]
      [(declaredInside? var (frontState bigStateList)) (cons (ChangeBindingInside var newVal (frontState bigStateList)) (followingStates bigStateList))]
      [else                                            (cons (frontState bigStateList) (ChangeBinding var newVal (followingStates bigStateList) funcList))])))

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
  (lambda (func bigFuncList)
    (cond
      ((null? bigFuncList)                            #f)
      ((declaredFunctionInside? func (frontState bigFuncList)) #t)
      (else                                            (declaredFunction? func (followingStates bigFuncList))))))

;declaredFunctionInside? - helper for declared? that dives into deeper states.
(define declaredFunctionInside?
  (lambda (var funcList)
    (cond
      ((null?  funcList)                  #f)
      ((equal?(variableDec funcList) funcList) #t)
      (else                                (declaredFunctionInside? funcList (followingStates funcList))))))

;AddFunctionBinding - takes a var name and the statelist, creates a new binding with given var.
(define AddFunctionBinding
  (lambda (func funcList)
    (if (declaredFunction? func funcList)
        (error 'Interpreter "Function already declared.")
        (cons (cons func (frontState funcList)) (followingStates funcList)))))

;CheckFunctionBinding - takes a var name and statelist, then returns the value of the variable. returns the first instance of said variable
(define CheckFunctionBinding
  (lambda (func bigFuncList)
    (cond
      ((null? bigFuncList)                                                         (error 'Interpreter "Function has not been declared."))
      ((equal? (frontState (CheckFunctionBindingInside func (frontState bigFuncList))) func) (followingStates (CheckFunctionBindingInside func (frontState bigFuncList)))) ;change abstract?
      (else                                                                         (CheckFunctionBinding func (followingStates bigFuncList))))))

;CheckFunctionBindingInside - takes a sub-stateList, and returns the binding of a corresponding variable if it exists. (var varValue)
(define CheckFunctionBindingInside
  (lambda (func funcList)
    (cond
      ((null? funcList)                     emptyReturn)
      ((equal? (variableDec funcList) func) (frontState funcList))
      (else                                 (CheckFunctionBindingInside func (followingStates funcList))))))

;
;; M-state related checks
;

;M-integer - checks what kind of operation needs to be performed, returns an integer.
(define M-integer
  (lambda (lis stateList funcList)
    (cond
      [(number? lis)                 lis]
      [(not (list? lis))             (CheckBinding lis stateList)]
      [(eq? (operator lis) 'funcall) (M-funcall lis stateList funcList (lambda (v f) v))]
      [(eq? (operator lis) '+)       (+ (M-integer (leftoperand lis) stateList funcList) (M-integer (rightoperand lis) stateList funcList))]
      [(and (eq? (operator lis) '-)  (null? (value lis))) (- 0 (M-integer (leftoperand lis) stateList funcList))]
      [(eq? (operator lis) '-)       (- (M-integer (leftoperand lis) stateList funcList) (M-integer (rightoperand lis) stateList funcList))]
      [(eq? (operator lis) '*)       (* (M-integer (leftoperand lis) stateList funcList) (M-integer (rightoperand lis) stateList funcList))]
      [(eq? (operator lis) '/)       (quotient (M-integer (leftoperand lis) stateList funcList) (M-integer (rightoperand lis) stateList funcList))]
      [(eq? (operator lis) '%)       (remainder (M-integer (leftoperand lis) stateList funcList) (M-integer (rightoperand lis) stateList funcList))]
      [else                          (error 'Interpreter "M-integer_Error")])))

;M-boolean - checks what kind of comparison must be made, returns either #t or #f.
(define M-boolean
  (lambda (lis stateList funcList next)
    (cond
      [(eq? lis 'true)          #t]
      [(eq? lis 'false)         #f]
      [(not (list? lis))        (CheckBinding lis stateList)]
      [(eq? (operator lis) '&&) (and (M-boolean (leftoperand lis) stateList funcList next) (M-boolean (rightoperand lis) stateList funcList next))]
      [(eq? (operator lis) '||) (or (M-boolean (leftoperand lis) stateList funcList next) (M-boolean (rightoperand lis) stateList funcList next))]
      [(eq? (operator lis) '!)  (not (M-boolean (leftoperand lis) stateList funcList next))]
      [(eq? (operator lis) '==) (eq? (M-expression (leftoperand lis) stateList funcList next) (M-expression (rightoperand lis) stateList funcList next))]
      [(eq? (operator lis) '>)  (> (M-expression (leftoperand lis) stateList funcList next) (M-expression (rightoperand lis) stateList funcList next))]
      [(eq? (operator lis) '<)  (< (M-expression (leftoperand lis) stateList funcList next) (M-expression (rightoperand lis) stateList funcList next))]
      [(eq? (operator lis) '<=) (<= (M-expression (leftoperand lis) stateList funcList next) (M-expression (rightoperand lis) stateList funcList next))]
      [(eq? (operator lis) '>=) (>= (M-expression (leftoperand lis) stateList funcList next) (M-expression (rightoperand lis) stateList funcList next))]
      [(eq? (operator lis) '!=) (not (eq? (M-expression (leftoperand lis) stateList funcList next) (M-expression (rightoperand lis) stateList funcList next)))]
      [else                     (error 'Interpreter "M-boolean_Error")])))
      

;M-return - prints out the requested return value. Makes sure that #t/#f becomes 'true and 'false as well.
(define M-return
  (lambda (statement stateList funcList next)
    (cond
      ((null? statement)                           (error 'Interpreter "M-return error - Null statement somehow"))
      ((number? (returnVal statement))             (returnVal statement))
      ((eq? #t (returnVal statement))              'true)
      ((eq? #f (returnVal statement))              'false)
      ((pair? (returnVal statement))               (M-return (returnify (M-expression (returnVal statement) stateList funcList initialNext)) stateList funcList next)) ;if an expression, call m-expression
      ((declared? (returnVal statement) stateList) (M-return (returnify (CheckBinding (returnVal statement) stateList)) stateList funcList next)) ;check if statement is a declared variable, if so return the value.
      (else                                        (error 'Interpreter "M-return error - Not accounted for")))))

;END
;(parser "testthis.txt")
(interpret "testthis.txt")
 