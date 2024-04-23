; Simple Language Interpreter Part 4

#lang racket
(require "classParser.rkt")


; An interpreter for the simple language that uses call/cc for the continuations.
; Does not handle side effects.

; Create helper functions to create a class closure and an instance closure and to access the members
; of the class closure and instance closure. The class closure must contain the parent/super class,
; the list of instance field names and the expressions that compute their initial values (if any),
; the list of methods/function names and closures, and (optionally) a list of class field names/values
; and a list of constructors. You may use your state/environment structure for each of these lists.
; The instance closure must contain the instance's class (i.e. the run-time type or the true type)
; and a list of instance field values.
(define (make-class-closure parent instance-field-names initial-values method-names method-closures class-field-names class-field-values constructors)
  (hash 'parent parent
        'instance-field-names instance-field-names
        'initial-values initial-values
        'method-names method-names
        'method-closures method-closures
        'class-field-names class-field-names
        'class-field-values class-field-values
        'constructors constructors))

(define (make-instance-closure class instance-field-values)
  (hash 'class class
        'instance-field-values instance-field-values))

(define (get-class-members class-closure)
  (hash-values class-closure))

(define (get-instance-members instance-closure)
  (hash-values instance-closure))

(define class-def-name car)

(define (interpret-class-list class-list environment)
  (if (null? class-list)
      environment
      (let* ((class-def (car class-list))
             (class-name (class-def-name class-def))
             (class-closure (make-class-closure class-def)))
        (interpret-class-list (cdr class-list) (insert class-name class-closure environment)))))


; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

(define result car)
(define return-environment cadr)
(define class-defs caddr)

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.
; The returned value is in the environment, at the front.
(define interpret
  (lambda (file class)
    (let* ((parsed (parser file))
           (global-env (create-global-env parsed (newenvironment)))
           (class-env (interpret-class-list (class-defs parsed) global-env))
           (class-closure (make-class-closure class '() '() '() '() '() '() '())))
      (scheme->language
       ; Call main function with no arguments
       (result (interpret-main class-env class-closure))
               class-closure))))

(define interpret-main
  (lambda (class-env class-closure)
    (let* ((class-name (get-class-members class-closure))
           (main-method (lookup-in-env 'main class-env)))
      (if (null? main-method)
          (myerror "Main method not found in class" class-name)
          (interpret-function '(funcall main) class-env
                              (lambda (v) (myerror "Uncaught exception thrown" v)))))))

; interprets a list of statements. The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list)
        (interpret-statement (car statement-list) environment return break continue throw)
            return break continue throw))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw))
      ; Function call resembles a block: '(function main (args) (body))
      ((eq? 'function (statement-type statement)) (declare-function statement environment))
      ; Function call pops the environment after the function call is done
      ; and updates the environment with the new bindings from the function call.
      ; The function call is interpreted with a new return continuation that updates the environment
      ; with the new bindings.
      ((eq? 'funcall (statement-type statement)) (extend-environment (pop-frame (return-environment
                                    (interpret-function statement environment throw))) environment))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw))
      (else (myerror "Unknown statement:" (statement-type statement))))))


;--------------------------------
; FUNCTION STUFF
;--------------------------------

; hack, because I don't know how else to do it
(define make-closure list)

; Updates the environment with the new bindings from the function call.
; [Andrej]: I may go back and abstract this a bit better
(define extend-environment
  (lambda (function-environment previous-environment)
    (cond
      ((null? function-environment) previous-environment)
      (else (extend-environment (cdr function-environment) (update-frame (variables (car function-environment))
                                      (cddr (car function-environment)) previous-environment '()))))))

; Connamacher said we should do a first global pass.
; This function creates the global environment by interpreting each statement in the statement list
; that is not a function declaration.
(define create-global-env
  (lambda (statement-list environment)
    (if (null? statement-list)
        environment
        (let ((statement (car statement-list)))
          (cond
            ((eq? 'class (statement-type statement))
             (create-global-env (cdr statement-list) (interpret-class-list statement environment)))
            (else
             (create-global-env (cdr statement-list) (global-interpret-statement statement environment))))))))

; interpret a statement in the first pass through of the parsed code
(define global-interpret-statement
  (lambda (statement environment)
    (cond
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment))
      ((eq? 'function (statement-type statement)) (declare-function statement environment))
      (else (myerror "Unknown statement:" (statement-type statement))))))

; The new environment is built by looking up the variables in the future environment.
; This is used to build the environment for a function call.
; The function call is interpreted with a new return continuation that updates the environment with the new bindings.
(define construct-future-env
  (lambda (variables-in-present-environment)
    (lambda (future-environment)
      (list variables-in-present-environment (rebuild-env-from-variables variables-in-present-environment future-environment)))))

; helper, rebuilds the environment from the variables in the present environment
; and the values in the future environment
(define rebuild-env-from-variables
  (lambda (variables environment)
    (if (null? variables)
        '()
        (cons (lookup (car variables) environment) (rebuild-env-from-variables (cdr variables) environment)))))       
          
; Updates the environment with the new bindings from the function call.
; [Andrej]: I REALLY need abstractions here. This is getting out of hand.
(define update-frame
  (lambda (function-var-list function-val-list previous-environment limbo)
    (if (or (null? function-var-list) (null? (cdr function-var-list)))
      previous-environment
      (cond
        ((exists? (caar function-var-list) previous-environment)
         (update-frame (cdr function-var-list) (cdr function-val-list)
         ; If the variable is in the previous environment, update it
         ; limbo is a list of variables that are not in the previous environment
                       (update (car function-var-list) (car function-val-list) previous-environment)) limbo)
        ((and (pair? (caar function-var-list)) (eq? '= (caaar function-var-list))) (update-frame (cdr function-var-list) function-val-list (update (car (cdaar function-var-list)) (find-in-limbo (car (cddaar function-var-list)) limbo) previous-environment) limbo))
        ((null? function-val-list) (myerror "Unknown variable" (car function-var-list)))
        (else (update-frame (cdr function-var-list) '() previous-environment (cons (cons (caar function-var-list) function-val-list) limbo)))))))

(define find-in-limbo
  (lambda (var limbo)
    (cond
      ((null? limbo) (myerror "Unknown variable (not even in limbo): " var))
      ((eq? var (caar limbo)) (car(cdar limbo)))
      (else (find-in-limbo var (cdr limbo))))))


(define get-function-name cadr)
(define get-function-formal-parameters caddr)
(define get-function-body cadddr)
(define get-closure-params car)
(define get-closure-body cadr)
(define get-closure-environment caddr)
(define get-funcall-actual-parameters cddr)


; interprets a function declaration. The function is added to the environment as a closure.
(define declare-function
  (lambda (statement environment)
    (insert (get-function-name statement)
            (make-closure (get-function-formal-parameters statement)
                          (get-function-body statement)
                          (construct-future-env (cons (get-function-name statement) (variables (car environment))))) ; attach function name in env to enable recursion
            environment)))


; interprets a function call. The function is looked up in the environment and interpreted with the actual parameters.
(define interpret-function
  (lambda (statement environment throw)
    (call/cc
     (lambda (return)
       ((lambda (function-closure)
          (interpret-statement-list (get-closure-body function-closure)
                                    (bind-parameters (get-closure-params function-closure)
                                                     (get-funcall-actual-parameters statement)
                                                     (push-frame (list ((get-closure-environment function-closure) environment))) ; get-closure-environment might break! current implementation is just a list! recursion might break it for real!!
                                                     environment
                                                     throw)
                                    ;(lambda (s) (myerror "no return statement")) do we need this??? -probably not
                                    return
                                    (lambda (env) (myerror "Break used outside of loop"))
                                    (lambda (env) (myerror "Continue used outside of loop"))
                                    throw))
        (lookup (get-function-name statement) environment))))))

; binds function input values to formal parameters
(define bind-parameters
  (lambda (formal-params argument-list fstate environment throw)
    (if (null? formal-params)
        fstate
        (bind-parameters (cdr formal-params)
                         (cdr argument-list)
                         (insert (car formal-params) (eval-expression (car argument-list) environment) fstate)
                         environment
                         throw))))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return)
  ; Pop the environment before returning
    (return (list (eval-expression (get-expr statement) environment) environment))))

; Adds a new variable binding to the environment. There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment) environment)
        (insert (get-declare-var statement) 'novalue environment))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment) environment)))
      
; We need to check if there is an else condition. Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw)
    (cond
      ((eval-expression (get-condition statement) environment) (interpret-statement (get-then statement) environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw))
      (else environment))))

; Interprets a while loop. We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment)
                        ; If the condition is true, interpret the body and then loop again
                            (loop condition body (interpret-statement body environment return break
                                                      (lambda (env) (break (loop condition body env))) throw))
                         ; Otherwise return the environment
                         environment))))
          ; Start the loop
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block. The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw)
    (pop-frame (interpret-statement-list (cdr statement)
                                         ; Push a new frame for the block
                                         (push-frame environment)
                                         ; Adjust the return continuation to pop the frame
                                         return
                                         ; Adjust the break continuation to pop the frame
                                         ; Same for continue and throw
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) environment) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw. If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list 
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
; Update eval-expression to handle function calls.
(define eval-expression
  (lambda (expr environment)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) 
        (lookup-in-static-chain expr environment (cdr environment))) 
      ((eq? 'funcall (statement-type expr)) (result (interpret-function expr environment (lambda (v env) (myerror "Uncaught exception thrown")))))
      (else (eval-operator expr environment)))))

; Evaluate a binary (or unary) operator. Although this is not dealing with side effects,
; I have the routine evaluate the left operand first and then pass the result to eval-binary-op2
; to evaluate the right operand. This forces the operands to be evaluated in the proper order
; in case you choose to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment) environment)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal. We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (empty-frame))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define empty-frame
  (lambda ()
    '(() ())))

(define push-frame
  (lambda (environment)
    (cons (empty-frame) environment)))

(define pop-frame
  (lambda (environment)
    (cdr environment)))

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (car environment))) #t)
      (else (exists? var (cdr environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment. If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup. Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment (cdr environment))))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
; If not found, lookup in the static environment
(define lookup-in-env
  (lambda (var environment static-environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (car environment))) (lookup-in-frame var (car environment)))
      (else (lookup-in-static-chain var static-environment environment)) ; Look up in static chain
    )
  )
)

; Look up a variable in the static chain of environments
(define lookup-in-static-chain
  (lambda (var static-chain current-env)
    (cond
      ((null? static-chain) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (car static-chain))) 
       (lookup-in-frame var (car static-chain))) ; Found in static chain
      (else 
       (lookup-in-static-chain var (cdr static-chain) current-env)) ; Continue searching
    )
  )
)

(define butlast
  (lambda (lst)
    (if (null? (cdr lst))
        '()
        (cons (car lst) (butlast (cdr lst)))))
)


; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

(define lastframe
  (lambda (environment)
    (cond
      ((null? environment) '())
      ((not (pair? environment)) '())
      ((not (null? (cdr environment))) (lastframe (cdr environment)))
      (else (car environment)))))

(define remove-last
  (lambda (lst)
    (if (null? (cdr lst))
        '()
        (cons (car lst) (remove-last (cdr lst))))))
; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment. Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment. Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (car environment)) (cdr environment))
        (cons (car environment) (update-existing var val (cdr environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (scheme->language val) (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

(interpret "p4/test.java" "A")