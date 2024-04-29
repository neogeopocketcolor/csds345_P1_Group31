#lang racket
(require "functionParser.rkt")

#|

Alex Seidman - Avry Rechel
ads206 - ajr250
4/10/2024
CSDS 345
Project 3 - Imperitive Language Interpreter

|#

;;=====================================
;;Abstractions
;;=====================================

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
(define initialCTimeVars '(()))

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

;parametrizing
(define parametizeCheckCdr
  (lambda (val)
    (if (list? val)
        (cdr val)
        val)))

(define parametizeCheckCar
  (lambda (val)
    (if (list? val)
        (car val)
        val)))

;;=====================================
;;Proper Functions
;;=====================================

;; Create a class closure with parent, fields, methods, and constructors
(define (create-class-closure class classlist)
  (let* ((class-name (car class))
         (extends (cadr class))
         (parent-class (and extends (find-class classlist (cdr extends)))) ; Find parent class
         (members (caddr class))
         (fields (filter (lambda (x) (eq? 'var (car x))) members)) ; Extract fields
         (methods (filter (lambda (x) (or (eq? 'function (car x)) 
                                         (eq? 'static-function (car x)))) members)) ; Extract methods
         (constructors (filter (lambda (x) (eq? 'constructor (car x))) members)) ; Extract constructors (optional)
         (field-names (map car fields))
         (field-values (map cadr fields))
         (method-names-closures (map (lambda (x) (cons (cadr x) (cddr x))) methods)))
    (list class-name parent-class field-names field-values method-names-closures constructors)))

;; Create an instance closure
(define (create-instance-closure class fields)
  (list class fields))

;; Access member (field or method) from a class closure
(define (get-class-member closure member-name)
  (let ((fields (cadr closure))
        (methods (caddr closure)))
    (cond ((assoc member-name fields) => cdr) ; retrieve field value if you find it in fields
          ((assoc member-name methods) => cdr) ; retrieve method if you find it in methods
          (else (error 'Interpreter "Member not found in class")))))

;; Access field value from an instance closure
(define (get-instance-field closure field-name)
  (let ((fields (cadr closure)))
    (cdr (assoc field-name fields))))

;interpret command - required, parses the input file and executes the code.
(define interpret
  (lambda (filename)
    (call/cc
     (lambda (initialReturn)
       (M-state (parser filename) initialState initialFunc (lambda (s f) (initialReturn (M-funcall '(funcall main) (push s) (push f) initialNext))) initialBreak initialThrow initialReturn)))))

(define find-class (lambda (class-list class-name)
  (cond
    ((null? class-list) (error 'Interpreter "find-class: Class not found"))
    ((equal? (list (cadar class-list)) class-name) (create-class-closure (car class-list) class-list)) ; Access the correct sublist
    (else (begin
    (display (list (cadar class-list)))
    (display class-name)
    (display (cdr class-list))
    (display "\n")
    (find-class (cdr class-list) class-name)
    )
    )
  )
))
;M-state - updates the stateList based on the current command at the front of the list.
(define M-state
  (lambda (lis stateList funcList next break throw return cTime)
    (cond
      [(null? lis) (next stateList funcList)]
      [(eq? (command lis) '=)        (M-assign (statement lis) stateList funcList (lambda (s f) (next (M-state (nextStatement lis) s f next break throw return cTime) f)) cTime)]
      [(eq? (command lis) 'var)      (M-declare (statement lis) stateList funcList (lambda (s f) (next (M-state (nextStatement lis) s f next break throw return cTime) f)) cTime)]
      
      [(eq? (command lis) 'if)       (if (M-boolean (condition lis) stateList funcList next cTime)
                                         (M-state (statement1 lis) stateList funcList (lambda (s f) (next (M-state (nextStatement lis) s f next break throw return cTime) f) break throw return) break throw return cTime)
                                         (if (not (null? (M-else lis)))
                                             (M-state (statement2 lis) stateList funcList (lambda (s f) (next (M-state (nextStatement lis) s f next break throw return) f)) break throw return cTime)
                                             (next (M-state (nextStatement lis) stateList funcList next break throw return cTime) funcList)))]
      
      [(eq? (command lis) 'while)    (loop (condition lis) (body lis) stateList funcList (lambda (s f) (next (M-state (nextStatement lis) s funcList next break throw return cTime) funcList))
                                           (lambda (s f) (break (M-state (nextStatement lis) s funcList next break throw return cTime))) throw return cTime)]
      [(eq? (command lis) 'return)   (return (M-return (statement lis) stateList funcList next cTime))]
      
      [(eq? (command lis) 'begin)    (M-state (beginBody lis) (push stateList) (push funcList) (lambda (s f) (next (M-state (nextStatement lis) (pop s) (pop f) next break throw return cTime) f)) ; something here makes the stateList the return function
                                           (lambda (s f) (call/cc (lambda k (break (M-state (nextStatement lis) (pop s) (pop funcList) next k throw return cTime))))) throw return cTime)]
      
      [(eq? (command lis) 'try)      (M-state (lisBeginning (beginBody lis)) (push stateList) (push funcList) (lambda (s1) (if (null? (finallyPoint (finallyShortcut lis))) (next (M-state (nextStatement lis) (pop s1) funcList next break throw return cTime))
                                                                                                      (next (M-state (finallyShortcut lis) s1 funcList (lambda (s) (M-state (nextStatement lis) s funcList next break throw return cTime)) break throw return cTime)))) ;next, go to finally
                                              (lambda (s1) (M-state (finallyShortcut lis) s1 funcList (lambda (s) (next (M-state (nextStatement lis) s funcList next break throw return cTime))) break throw return cTime)) ;if broken, go to finally
                                              (lambda (e s f) (M-state (catchShortcut lis) (ChangeBinding (innerState (beginBody (catchShortcut lis))) e (AddBinding (innerState (beginBody (catchShortcut lis))) s funcList cTime) funcList cTime) ;if exception is thrown, go to catch
                                                                     (lambda (s1) (if (null? (finallyPoint (finallyShortcut lis))) (M-state (nextStatement lis) (pop s1) funcList next break throw return cTime)
                                                                                      (M-state (finallyShortcut lis) s1 funcList (lambda (s2) (M-state (nextStatement lis) s2 funcList next break throw return cTime)) break throw return cTime)));catch's next statement is finally
                                                                     break throw return cTime)) return cTime)] ;catch's break statement is finally
      
      [(eq? (command lis) 'catch)    (M-state (finallyPointAlt (beginBody lis)) stateList funcList (lambda (s) (next s)) (lambda (s) (next s)) throw return cTime)]
      [(eq? (command lis) 'throw)    (throw (cadar lis) stateList)]
      [(eq? (command lis) 'finally)  (M-state (lisBeginning (beginBody lis)) stateList funcList
                                              (lambda (s) (next (pop s)))
                                              (lambda (s) (next (pop s))) throw return cTime)] ;return popped state
      
      [(eq? (command lis) 'break)    (break stateList)]
      [(eq? (command lis) 'continue) (next stateList)]
      
      [(eq? (command lis) 'function) (M-declareFunction (statement lis) stateList funcList (lambda (s f) (next (M-state (nextStatement lis) stateList f next break throw return cTime) funcList)))]
      [(eq? (command lis) 'funcall)  (M-funcall (statement lis) (push stateList) (push funcList) (lambda (s f) (next (M-state (nextStatement lis) (pop s) (pop f) next break throw return cTime))) cTime)]
      [(eq? (command lis) 'class)    (create-class-closure (statement lis) stateList funcList (lambda (s f) (next (M-state (nextStatement lis) s f next break throw return funcList cTime))))]
      ; PLACE HOLDER!!!                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      [else                          (error 'Interpreter "Not a valid command")])))


;;=====================================
;; General Use
;;=====================================

;findGlobal
(define findGlobal
  (lambda (stateList)
    (if (null? (followingStates stateList)) (frontState stateList)
        (findGlobal (followingStates stateList)))))

;Helper function for loops
(define loop
  (lambda (condition body stateList funcList next break throw return cTime)
    (if (M-boolean condition stateList funcList next cTime)
        (M-state body stateList funcList
                 (lambda (s f) (loop condition body s funcList next break throw return cTime))
                 break throw return cTime)
        (next stateList funcList))))

;M-declare - declares a variable, either with or without a binding to a value.
(define M-declare
  (lambda (lis stateList funcList next cTime)
    (if (null? (value lis))
        (next (AddBinding (varValue lis) stateList funcList cTime)) ;declare only
        (M-assign lis (AddBinding (varValue lis) stateList funcList cTime) funcList next cTime)))) ;declare and assign

        
;M-assign - assigns a binding to a variable if the variable doesn't already have a value.
(define M-assign 
  (lambda (lis stateList funcList next cTime)
    (if (not (declared? (leftoperand lis) stateList cTime))
        (error 'Interpreter "Variable not declared. :(")
        (next (ChangeBinding (leftoperand lis) (M-expression (rightoperand lis) stateList funcList next cTime) stateList funcList cTime) funcList))))

;M-declareFunction - declares a function, binding the function's name, (formal parameters), and (comamands), into one readable lis.
(define M-declareFunction
  (lambda (lis stateList funcList next)
    (next stateList (AddFunctionBinding (cdr lis) funcList))))

;M-funcall - handles the calling of a function. Finds if the function's name exists in stateList, and if it does
(define M-funcall
  (lambda (lis stateList funcList next cTime)
    (call/cc
     (lambda (initialReturn)
       (next (M-state (commandList (CheckFunctionBinding (leftoperand lis) funcList)) ;commands (lis)
                      (list (parametize (paramList (CheckFunctionBinding (leftoperand lis) funcList)) (cddr lis) stateList funcList next cTime) (findGlobal stateList)) ;stateList
                      (push (list (findGlobal funcList))) ;funcList
                      initialNext initialBreak initialThrow initialReturn cTime)
             funcList)))))

;parametize - takes list of formal and actual parameters and declares the formal parameters accordingly to the proper environment
(define parametize
  (lambda (formal actual stateList funcList next cTime)
    (cond
      [(and (null? formal) (not (null? actual))) (error 'Interpreter "Formal paremeters does not match number of actual paremeters.")]
      [(null? formal)                            (frontState stateList)]
      [else                                      (parametize (cdr formal) (parametizeCheckCdr actual)
                                                             (M-declare (cons 'var (list (car formal) (M-expression (parametizeCheckCar actual) (followingStates stateList) funcList next cTime))) stateList funcList next cTime)
                                                             funcList next cTime)])))


;;=====================================
;;Variable Declare/Assign/Change
;;=====================================

;declared? - takes a var name and the stateList, returning #t if var name exists in statelist. ex: (declared? 'x ((x 3))) returns #t.
(define declared?
  (lambda (var bigStateList cTime)
    (cond
      ((null? bigStateList)                                  #f)
      ((declaredInside? var (frontState bigStateList) cTime) #t)
      (else                                                  (declared? var (followingStates bigStateList) cTime)))))

;delcaredInside? - helper for declared? that dives into deeper states.
(define declaredInside?
  (lambda (var stateList cTime)
    (cond
      ((null?  stateList)                  #f)
      ((equal?(variableDec stateList) var) #t)
      (else                                (declaredInside? var (followingStates stateList) cTime)))))

;AddBinding - takes a var name and the statelist, creates a new binding with given var.
(define AddBinding
  (lambda (var stateList cTime)
    (if (declaredInside? var (frontState stateList) cTime)
        (error 'Interpreter "Variable already declared.")
        (cons (cons (list var 'null) (frontState stateList)) (followingStates stateList)))))

;CheckBinding - takes a var name and statelist, then returns the value of the variable. returns the first instance of said variable
(define CheckBinding
  (lambda (var bigStateList cTime)
    (cond
      ((null? bigStateList)                                                               (error 'Interpreter "Variable has not been declared."))
      ((equal? (frontState (CheckBindingInside var (frontState bigStateList) cTime)) var) (innerFollowingStates (CheckBindingInside var (frontState bigStateList) cTime)))
      (else                                                                               (CheckBinding var (followingStates bigStateList) cTime)))))

;CheckBindingInside - takes a sub-stateList, and returns the binding of a corresponding variable if it exists. (var varValue)
(define CheckBindingInside
  (lambda (var stateList cTime)
    (cond
      ((null? stateList)                     emptyReturn)
      ((equal? (variableDec stateList) var) (frontState stateList))
      (else                                 (CheckBindingInside var (followingStates stateList) cTime)))))


;ChangeBinding - takes a var name, value, and stateList, then returns the stateList with the new variable's value updated.
(define ChangeBinding
  (lambda (var newVal bigStateList funcList cTime)
    (cond
      [(null? bigStateList)                                  (error `Interpreter "Variable has not been declared.")]
      [(declaredInside? var (frontState bigStateList) cTime) (cons (ChangeBindingInside var newVal (frontState bigStateList) cTime) (followingStates bigStateList))]
      [else                                                  (cons (frontState bigStateList) (ChangeBinding var newVal (followingStates bigStateList) funcList cTime))])))

;ChangeBindingInside - helper for ChangeBinding for deeper states.
(define ChangeBindingInside
  (lambda (var newVal stateList cTime)
    (cond
      ((null? stateList)                     stateList)
      ((equal? (variableDec stateList) var) (cons (list var newVal) (followingStates stateList)))
      (else                                 (cons (frontState stateList) (ChangeBindingInside var newVal (followingStates stateList) cTime))))))

;;=====================================
;;Function Declare/Assign/Change
;;=====================================

;declaredFunction? - takes a var name and the stateList, returning #t if var name exists in statelist. ex: (declared? 'x ((x 3))) returns #t.
(define declaredFunction?
  (lambda (func bigFuncList)
    (cond
      ((null? bigFuncList)                                     #f)
      ((declaredFunctionInside? func (frontState bigFuncList)) #t)
      (else                                                   (declaredFunction? func (followingStates bigFuncList))))))

;declaredFunctionInside? - helper for declared? that dives into deeper states.
(define declaredFunctionInside?
  (lambda (var funcList)
    (cond
      ((null?  funcList)                       #f)
      ((equal?(variableDec funcList) funcList) #t)
      (else                                    (declaredFunctionInside? funcList (followingStates funcList))))))

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
      ((null? bigFuncList)                                                                   (error 'Interpreter "Function has not been declared."))
      ((equal? (frontState (CheckFunctionBindingInside func (frontState bigFuncList))) func) (followingStates (CheckFunctionBindingInside func (frontState bigFuncList)))) ;change abstract?
      (else                                                                                  (CheckFunctionBinding func (followingStates bigFuncList))))))

;CheckFunctionBindingInside - takes a sub-stateList, and returns the binding of a corresponding variable if it exists. (var varValue)
(define CheckFunctionBindingInside
  (lambda (func funcList)
    (cond
      ((null? funcList)                     emptyReturn)
      ((equal? (variableDec funcList) func) (frontState funcList))
      (else                                 (CheckFunctionBindingInside func (followingStates funcList))))))

;;=====================================
;; M-state related checks
;;=====================================

;M-expression - checks if an operation needs to return a number (math equation) or a boolean (t/f).
(define M-expression
  (lambda (lis stateList funcList next cTime)
    (cond
      [(not (list? lis))                   (if (math? lis)
                                              (M-integer lis stateList funcList cTime)
                                              (M-boolean lis stateList funcList next cTime))]
      [(declared? lis stateList cTime)     (M-expression (CheckBinding lis stateList cTime) stateList funcList next cTime)]
      [(math? (operator lis))              (M-integer lis stateList funcList cTime)]
      [(eq? (operator lis) 'funcall)       (getReturnValue (M-funcall lis (push stateList) (push funcList) (lambda (v f) v) cTime) cTime)] 
      [else                                (M-boolean lis stateList funcList next cTime)])))

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

;M-integer - checks what kind of operation needs to be performed, returns an integer.
(define M-integer
  (lambda (lis stateList funcList cTime)
    (cond
      [(number? lis)                 lis]
      [(not (list? lis))             (CheckBinding lis stateList cTime)]
      [(eq? (operator lis) 'funcall) (getReturnValue (M-funcall lis (push stateList) (push funcList) (lambda (v f) v) cTime) cTime)]
      [(eq? (operator lis) '+)       (+ (M-integer (leftoperand lis) stateList funcList cTime) (M-integer (rightoperand lis) stateList funcList cTime))]
      [(and (eq? (operator lis) '-)  (null? (value lis))) (- 0 (M-integer (leftoperand lis) stateList funcList cTime))]
      [(eq? (operator lis) '-)       (- (M-integer (leftoperand lis) stateList funcList cTime) (M-integer (rightoperand lis) stateList funcList cTime))]
      [(eq? (operator lis) '*)       (* (M-integer (leftoperand lis) stateList funcList cTime) (M-integer (rightoperand lis) stateList funcList cTime))]
      [(eq? (operator lis) '/)       (quotient (M-integer (leftoperand lis) stateList funcList cTime) (M-integer (rightoperand lis) stateList funcList cTime))]
      [(eq? (operator lis) '%)       (remainder (M-integer (leftoperand lis) stateList funcList cTime) (M-integer (rightoperand lis) stateList funcList cTime))]
      [else                          (error 'Interpreter "M-integer_Error")])))

;M-boolean - checks what kind of comparison must be made, returns either #t or #f.
(define M-boolean
  (lambda (lis stateList funcList next cTime)
    (cond
      [(or (eq? lis 'true) (eq? lis #t))          #t]
      [(or (eq? lis 'false) (eq? lis #f))         #f]
      [(not (list? lis))        (CheckBinding lis stateList cTime)]
      [(eq? (operator lis) 'funcall) (getReturnValue (M-funcall lis (push stateList) (push funcList) (lambda (v f) v) cTime) cTime)]
      [(eq? (operator lis) '&&) (and (M-boolean (leftoperand lis) stateList funcList next cTime) (M-boolean (rightoperand lis) stateList funcList next cTime))]
      [(eq? (operator lis) '||) (or (M-boolean (leftoperand lis) stateList funcList next cTime) (M-boolean (rightoperand lis) stateList funcList next cTime))]
      [(eq? (operator lis) '!)  (not (M-boolean (leftoperand lis) stateList funcList next cTime))]
      [(eq? (operator lis) '==) (eq? (M-expression (leftoperand lis) stateList funcList next cTime) (M-expression (rightoperand lis) stateList funcList next cTime))]
      [(eq? (operator lis) '>)  (> (M-expression (leftoperand lis) stateList funcList next cTime) (M-expression (rightoperand lis) stateList funcList next cTime))]
      [(eq? (operator lis) '<)  (< (M-expression (leftoperand lis) stateList funcList next cTime) (M-expression (rightoperand lis) stateList funcList next cTime))]
      [(eq? (operator lis) '<=) (<= (M-expression (leftoperand lis) stateList funcList next cTime) (M-expression (rightoperand lis) stateList funcList next cTime))]
      [(eq? (operator lis) '>=) (>= (M-expression (leftoperand lis) stateList funcList next cTime) (M-expression (rightoperand lis) stateList funcList next cTime))]
      [(eq? (operator lis) '!=) (not (eq? (M-expression (leftoperand lis) stateList funcList next cTime) (M-expression (rightoperand lis) stateList funcList next cTime)))]
      [else                     (error 'Interpreter "M-boolean_Error")])))
      

;M-return - prints out the requested return value. Makes sure that #t/#f becomes 'true and 'false as well.
(define M-return
  (lambda (statement stateList funcList next cTime)
    (cond
      ((null? statement)                                 (error 'Interpreter "M-return error - Null statement somehow"))
      ((number? (returnVal statement))                   (returnBinding (returnVal statement) (pop stateList) funcList next cTime))
      ((or (eq? #t (returnVal statement)) (eq? 'true (returnVal statement)))  (returnBinding #t (pop stateList) funcList next cTime))
      ((or (eq? #f (returnVal statement)) (eq? 'false (returnVal statement))) (returnBinding #f (pop stateList) funcList next cTime))
      ((pair? (returnVal statement))                     (M-return (returnify (M-expression (returnVal statement) stateList funcList initialNext cTime)) stateList funcList next cTime)) ;if an expression, call m-expression
      ((declared? (returnVal statement) stateList cTime) (M-return (returnify (CheckBinding (returnVal statement) stateList cTime)) stateList funcList next cTime)) ;check if statement is a declared variable, if so return the value.
      (else                                              (error 'Interpreter "M-return error - Not accounted for")))))


(define returnBinding
  (lambda (toReturn stateList funcList next cTime)
    (if (null? toReturn)
        (M-assign (list (cons 'var (list 'returnValue123 'null))) stateList funcList next cTime)
        (M-assign (list (cons 'var (list 'returnValue123 toReturn))) stateList funcList next cTime))))

(define getReturnValue
  (lambda (stateList cTime)
    (CheckBinding 'returnValue123 stateList cTime)))

(define mainReturn
  (lambda (stateList cTime)
    (cond
      [(eq? #t (getReturnValue stateList cTime)) 'true]
      [(eq? #f (getReturnValue stateList cTime)) 'false]
      [else (getReturnValue stateList cTime)])))
  

;END
;(parser "testthis.txt")
(interpret "testthis.txt")
 