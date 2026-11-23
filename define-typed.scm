; SPDX-FileCopyrightText: 2026 Arne Babenhauserheide
;
; SPDX-License-Identifier: MIT

(define-module (define-typed)
  #:export (define-typed lambda-typed define-typed* lambda-typed*)
  #:use-module (srfi srfi-11)
  #:re-export (let-values))

;; common procedures
(define-inlinable (takes-single-value? proc)
  (equal? '(1 0 #f) (procedure-minimum-arity proc)))

(define-syntax-rule (call-and-check-return-type proc ret?)
  (if ret? ;; #f means: do not check
      ;; get the result
      (let ((res (proc)))
        ;; typecheck the result
        ;; TODO report a bug in Guile that this causes trouble with => in cond when using define-inlinable
        ;; TODO can no longer reproduce this problem.
        (unless (ret? res)
          (error "type error: return value ~a does not match ~a"
                 res ret?))
        ;; return the result
        res)
      (proc)))

(define-syntax-rule
  (call-and-check-return-type/proc proc check-values)
  ;; get the result
  (let-values ((res (proc)))
    ;; typecheck the result
    (unless (check-values res)
      (error "type error: return values ~a do not match ~a"
             res check-values))
    ;; return the result
    (apply values res)))
(define-syntax-rule
  (call-and-check-return-type/multiple proc return-checkers)
  ;; get the result
  (let-values ((res (proc)))
    ;; typecheck the result
    (let loop ((check return-checkers) (r res))
      (when (pair? check)
        (unless ((car check) (car r))
          (error "type error: return values ~a do not match ~a"
                 res return-checkers))
        (loop (cdr check) (cdr r))))
    ;; return the result
    (apply values res)))

(define-syntax-rule (check-argument-and-type-count args types)
  (unless (null? types) ;; allow untyped args with return type check
    (let loop ((a args) (t types))
      (unless (equal? (pair? a) (pair? t))
        ;; (when (and (pair? a) (not (pair? (cdr a))))
        ;; a is one element longer than t ⇒ no return type
        ;; TODO move such a check ^ into a guard of a syntax rule.
        (error "argument error: number of arguments ~a and types ~a differs"
               args types))
      (when (pair? a)
        (loop (cdr a) (cdr t))))))

;; inlinable add-properties!, because guild compile -W3 define-typed.scm
;; marks it as possibly unused otherwise.
(define-inlinable (add-properties! proc name from-proc type-properties ret? rest? types)
  ;; add procedure properties via an inner procedure
  (set-procedure-properties! proc (procedure-properties from-proc))
  ;; record the types
  (set-procedure-property! proc 'return-types ret?)
  (set-procedure-property! proc 'argument-types types)
  (set-procedure-property! proc 'rest-types rest?)
  ;; add displayed types
  (set-procedure-property! proc 'return-types-display
                           (procedure-property type-properties 'return-types-display))
  (set-procedure-property! proc 'argument-types-display
                           (procedure-property type-properties 'argument-types-display))
  (set-procedure-property! proc 'rest-types-display
                           (procedure-property type-properties 'rest-types-display))
  ;; preserve the name
  (set-procedure-property! proc 'name name))


;; specific to define-typed
(define-syntax check-types
  (syntax-rules ()
    ((_ (#f types? ...) (argument arguments ...)) ;; no type check
       (check-types (types? ...) (arguments ...)))
    ((_ (type? types? ...) (argument arguments ...))
     (begin
       (unless (type? argument)
         (error "type error ~a ~a" type? argument))
       (check-types (types? ...) (arguments ...))))
    ((_ () (arguments ...)) #f))) ;; untyped arguments are legal when there’s a return type


(define-syntax typed/base
  (syntax-rules ()
    ((_ (args ...) (types ...)
        #f ret-proc ret-values
        def lamb check ;; define or define*, ...
        body ...)
     (begin
       (check-argument-and-type-count
        (quote (args ...)) (quote (types ...)))
       (lamb (args ...)
             ;; create a sub-procedure to run after typecheck
             (def inner (lamb () body ...))
             ;; typecheck the arguments
             (check (types ...) (args ...))
             ;; get and check the result
             (ret-proc inner ret-values))))
    ((_ (args ... . rest) (types ...)
        rest? ret-proc ret-values
        def lamb check ;; define or define*, ...
        body ...)
     (begin
       (check-argument-and-type-count
        (quote (args ...)) (quote (types ...)))
       ;; bind rest? to avoid properties-calling at execution time
       (let ((bound-rest? rest?))
         (lamb (args ... . rest)
               ;; create a sub-procedure to run after typecheck
               (def inner (lamb () body ...))
               ;; typecheck the arguments
               (check (types ...) (args ...))
               ;; and the rest argument
               (check (bound-rest?) (rest))
               ;; get and check the result
               (ret-proc inner ret-values)))))))

(define-syntax define-typed/base
  (syntax-rules ()
    ((_ #f ;; no name
        (args ...) (types ...)
        #f ret-proc ret-values
        def lamb check ;; define or define*, ...
        body ...)
     (begin
       (let ((proc
              (typed/base
               (args ...) (types ...)
               #f ret-proc ret-values
               def lamb check ;; define or define*, ...
               body ...))
             (properties-helper (lamb (args ...) body ...))
             (type-properties-helper (lambda ()
                                       #((return-types-display ret-values)
                                         (argument-types-display types ...)
                                         (rest-types-display rest?))
                                       #f)))
         ;; add properties to the defined procedure
         (add-properties! proc #f properties-helper type-properties-helper
                          ret-values #f (list types ...))
         proc)))
    ((_ #f ;; no name
        (args ... . rest) (types ...)
        rest? ret-proc ret-values
        def lamb check ;; define or define*, ...
        body ...)
     (begin
       (let ((proc
              (typed/base
               (args ...) (types ...)
               rest? ret-proc ret-values
               def lamb check ;; define or define*, ...
               body ...))
             (properties-helper (lamb (args ...) body ...))
             (type-properties-helper (lambda ()
                                       #((return-types-display ret-values)
                                         (argument-types-display types ...)
                                         (rest-types-display rest?))
                                       #f)))
         ;; add properties to the defined procedure
         (add-properties! proc #f properties-helper type-properties-helper
                          ret-values rest? (list types ...))
         proc)))
    ((_ procname ;; with name
        (args ...) (types ...) ;; no rest argument
        #f ret-proc ret-values
        def lamb check ;; define or define*, ...
        body ...)
     (begin
       (def procname
            (typed/base
             (args ...) (types ...)
             #f ret-proc ret-values
             def lamb check ;; define or define*, ...
             body ...))
       (let ((properties-helper (lamb (args ...) body ...))
             (type-properties-helper (lambda ()
                                       #((return-types-display ret-values)
                                         (argument-types-display types ...)
                                         (rest-types-display rest?))
                                       #f)))
         ;; add properties to the defined procedure
         (add-properties! procname (and procname 'procname) properties-helper type-properties-helper
                          ret-values #f (list types ...)))))
    ((_ procname ;; with name
        (args ... . rest) (types ...) ;; with rest argument
        rest? ret-proc ret-values
        def lamb check ;; define or define*, ...
        body ...)
     (begin
       (def procname
            (typed/base
             (args ... . rest) (types ...)
             rest? ret-proc ret-values
             def lamb check ;; define or define*, ...
             body ...))
       (let ((properties-helper (lamb (args ... . rest) body ...))
             (type-properties-helper (lambda ()
                                       #((return-types-display ret-values)
                                         (argument-types-display types ...)
                                         (rest-types-display rest?))
                                       #f)))
         ;; add properties to the defined procedure
         (add-properties! procname (and procname 'procname) properties-helper type-properties-helper
                          ret-values rest? (list types ...)))))))

;; helper without keyword support
(define-syntax define-typed/helper
  (syntax-rules ()
    ((_ procname
        (args ...) (types ...)
        #f ret-proc ret-values
        body ...)
     (define-typed/base procname
       (args ...) (types ...)
       #f ret-proc ret-values
       define lambda check-types ;; without keywords
       body ...))
    ;; with rest arguments
    ((_ procname
        (args ... . rest) (types ...)
        rest? ret-proc ret-values
        body ...)
     (define-typed/base procname
       (args ... . rest) (types ...)
       rest? ret-proc ret-values
       define lambda check-types ;; without keywords
       body ...))))

;; helper to distinguish between first type being ret? and first type
;; being first argument
(define-syntax define-typed/compat
  (syntax-rules (copied-> reversed->)
    ((_ helper (procname copied-> a ... reversed-> ())
        (ret? copied-> t ... reversed-> ())
        body ...)
     (helper procname (a ...) (t ...)
             #f
             call-and-check-return-type
             ret?
             body ...))
    ;; transfer one argument and type each from reversed to copied; if
    ;; len args = len types, ret? is a return type check
    ((_ helper (procname copied-> a ... reversed-> (aa aa* ...))
        (ret? copied-> t ... reversed-> (tt tt* ...))
        body ...)
     (define-typed/compat helper (procname copied-> aa a ... reversed-> (aa* ...))
       (ret? copied-> tt t ... reversed-> (tt* ...))
        body ...))
    ;; one type less than arguments: ret? is a type check FIXME:
    ;; infinite loop?
    ((_ helper (procname copied-> a ... reversed-> (aa))
        (ret? copied-> t ... reversed-> ())
        body ...)
     (define-typed/compat helper (procname copied-> aa a ... reversed-> ())
       (#f copied-> ret? t ... reversed-> ())
        body ...))
    ;; re-reverse arguments into copied-> to be able to add ret? as
    ;; the last type if needed
    ((_ helper (procname copied-> a ... reversed-> (aa aa* ...))
        (ret? copied-> t ... reversed-> (tt tt* ...))
        body ...)
     (define-typed/compat helper (procname copied-> aa a ... reversed-> (aa* ...))
       (ret? copied-> tt t ... reversed-> (tt* ...))
        body ...))
    ;; reverse all arguments into reversed-> (treat arguments and
    ;; types separately because the number may be different)
    ((_ helper (procname a args ... copied-> reversed-> (aa* ...))
        (ret? types ... copied-> reversed-> (tt* ...))
        body ...)
     (define-typed/compat helper (procname args ... copied-> reversed-> (a aa* ...))
       (ret? types ... copied-> reversed-> (tt* ...))
        body ...))
    ((_ helper (procname args ... copied-> reversed-> (aa* ...))
        (ret? t types ... copied-> reversed-> (tt* ...))
        body ...)
     (define-typed/compat helper (procname args ... copied-> reversed-> (aa* ...))
       (ret? types ... copied-> reversed-> (t tt* ...))
        body ...))
    ;; entry point for multiple arguments: first start reversing the arguments
    ((_ helper (procname a args ...)
        (ret? t types ...)
        body ...)
     (define-typed/compat helper (procname args ... copied-> reversed-> (a))
       (ret? types ... copied-> reversed-> (t))
        body ...))
    ;; shortcut: ret? is #f
    ((_ helper (procname args ...) (#f types ...)
        body ...)
     (define-typed/compat helper (procname copied-> args ... reversed-> ())
       (#f copied-> types ... reversed-> ())
        body ...))
    ;; shortcut: one argument, no type: ret? is a type check
    ;; => ret? must check the argument instead
    ((_ helper (procname a)
        (ret?)
        body ...)
     (define-typed/compat helper (procname copied-> a reversed-> ())
       (#f copied-> ret? reversed-> ())
        body ...))
    ;; neither types nor arguments: ret? is the return type.
    ((_ helper (procname)
        (ret?)
        body ...)
     (define-typed/compat helper (procname copied-> reversed-> ())
       (ret? copied-> reversed-> ())
        body ...))))

(define-syntax-rule (each-rest predicate)
  (let ((each-rest (lambda (args)
                     #((display predicate)) ;; gives a readable representation, cannot use name because that has more automation
                      (let loop ((args args))
                        (cond ((null? args) #t)
                              ((predicate (car args))
                               (loop (cdr args)))
                              (else #f))))))
    (set-procedure-property! each-rest 'kind 'each-rest)
    (set-procedure-property! each-rest 'value predicate)
    each-rest))

(define-syntax-rule (circular-rest predicates)
  (let ((circular-rest  (lambda (args)
                          #((display predicates)) ;; gives a readable representation, cannot use name because that has more automation
                          (let loop ((preds predicates) (args args))
                            (cond ((null? args) #t)
                                  ((null? preds) (loop predicates args)) ;; circular
                                  ((not ((car preds) (car args))) #f)
                                  (else (loop (cdr preds) (cdr args))))))))
    (set-procedure-property! circular-rest 'kind 'circular-rest)
    (set-procedure-property! circular-rest 'value predicates)
    circular-rest))


;; Define a procedure with typechecks.
(define-syntax define-typed
  (syntax-rules (-> ::)
    ;; syntax with -> ret
    ;; rest arguments without return value
    ;; check for each rest element
    ((_ (procname args ... . rest)
        ;; using the cons symbol :: from ocaml / ML languages to separate the rest types
        (types ... :: rest?)
        body ...)
     (define-typed/helper procname (args ... . rest)
       (types ...)
       (each-rest rest?)
       call-and-check-return-type
       #f
       body ...))
    ;; check all rest arguments with the rest?
    ((_ (procname args ... . rest)
        ;; using the cons symbol :: from ocaml / ML languages to separate the rest types
        (types ... (:: rest?))
        body ...)
     (define-typed/helper procname (args ... . rest)
       (types ...)
       rest?
       call-and-check-return-type
       #f
       body ...))
    ;; check rest arguments with circular rest?
    ((_ (procname args ... . rest)
        ;; using the cons symbol :: from ocaml / ML languages to separate the rest types
        (types ... (:: rest1? rest2? rest*? ...))
        body ...)
     (define-typed/helper procname (args ... . rest)
       (types ...)
       (circular-rest (list rest1? rest2? rest*? ...))
       call-and-check-return-type
       #f
       body ...))
    ;; each rest argument with single return value
    ((_ (procname args ... . rest)
        ;; using the cons symbol :: from ocaml / ML languages to separate the rest types
        (types ... :: rest? -> ret?)
        body ...)
     (define-typed/helper procname (args ... . rest)
       (types ...)
       (each-rest rest?)
       call-and-check-return-type
       ret?
       body ...))
    ;; alternate single return value syntax with -> ret and check rest arguments with one proc
    ((_ (procname args ... . rest)
        ;; using the cons symbol :: from ocaml / ML languages to separate the rest types
        (types ... (:: rest?) -> ret?)
        body ...)
     (define-typed/helper procname (args ... . rest)
       (types ...)
       rest?
       call-and-check-return-type
       ret?
       body ...))
    ;; alternate single return value syntax with -> ret and check rest arguments with circular rest?
    ((_ (procname args ... . rest)
        ;; using the cons symbol :: from ocaml / ML languages to separate the rest types
        (types ... (:: rest1? rest2? rest*? ...) -> ret?)
        body ...)
     (define-typed/helper procname (args ... . rest)
       (types ...)
       (circular-rest (list rest1? rest2? rest*? ...))
       call-and-check-return-type
       ret?
       body ...))
    ;; alternate single return value syntax with -> ret and check rest arguments with circular rest?
    ((_ (procname args ... . rest)
        ;; using the cons symbol :: from ocaml / ML languages to separate the rest types
        (types ... (:: rest1? rest2? rest*? ...) -> ret?)
        body ...)
     (define-typed/helper procname (args ... . rest)
       (types ...)
       (circular-rest (list rest1? rest2? rest*? ...))
       call-and-check-return-type
       ret?
       body ...))
    ;; single checker: check all returned values via procedure
    ((_ (procname args ...)
        ((-> ret?) types ...)
        body ...)
     (define-typed/helper procname (args ...)
       (types ...)
       #f
       call-and-check-return-type/proc
       ret?
       body ...))
    ;; with rest arguments – using the original form with (-> ret?) at the end
    ((_ (procname args ... . rest)
        (types ... :: rest? (-> ret?))
        body ...)
     (define-typed/helper procname (args ... . rest)
       (types ...)
       (each-rest rest?)
       call-and-check-return-type/proc
       ret?
       body ...))
    ((_ (procname args ... . rest)
        (types ... (:: rest?) (-> ret?))
        body ...)
     (define-typed/helper procname (args ... . rest)
       (types ...)
       rest?
       call-and-check-return-type/proc
       ret?
       body ...))
    ((_ (procname args ... . rest)
        (types ... (:: rest1? rest2? rest*? ...) (-> ret?))
        body ...)
     (define-typed/helper procname (args ... . rest)
       (types ...)
       (circular-rest (list rest1? rest2? rest*? ...))
       call-and-check-return-type/proc
       ret?
       body ...))
    ;; single return value syntax with -> ret
    ((_ (procname args ...)
        (types ... -> ret?)
        body ...)
     (define-typed/helper procname (args ...)
       (types ...)
       #f ;; no rest arguments
       call-and-check-return-type
       ret?
       body ...))
    ;; single -> checker: check all returned values via procedure
    ((_ (procname args ...)
        (types ... (-> ret?))
        body ...)
     (define-typed (procname args ...)
       ((-> ret?) types ...)
        body ...))
    ;; two or more return checkers: one per value (fixed number of
    ;; return values!)
    ((_ (procname args ...)
        (types ... (-> ret1? ret2* ret*? ...))
        body ...)
     (define-typed (procname args ...)
       ((-> ret1? ret2* ret*? ...) types ...)
       body ...))
    ;; alternate single return value syntax with rest argument and -> ret
    ((_ (procname args ... . rest)
        (types ... -> ret?)
        body ...)
     (define-typed/helper procname (args ... . rest)
       (types ...)
       #f ;; no rest argument check
       call-and-check-return-type
       ret?
       body ...))
    ;; two or more return checkers: one per value (fixed number of
    ;; return values!)
    ((_ (procname args ...)
        ((-> ret1? ret2? ret*? ...) types ...)
        body ...)
     (define-typed/helper procname (args ...)
       (types ...)
       #f ;; TODO three more blocks for rest? support
       call-and-check-return-type/multiple
       (list ret1? ret2? ret*? ...) ;; return-checkers
       body ...))
    ;; with rest arguments – using the original form with (-> (list ret1? ret2? ret*? ...) ...) at the end
    ((_ (procname args ... . rest)
        (types ... :: rest? (-> ret1? ret2? ret*? ...))
        body ...)
     (define-typed/helper procname (args ... . rest)
       (types ...)
       (each-rest rest?)
       call-and-check-return-type/multiple
       (list ret1? ret2? ret*? ...)
       body ...))
    ((_ (procname args ... . rest)
        (types ... (:: rest?) (-> ret1? ret2? ret*? ...))
        body ...)
     (define-typed/helper procname (args ... . rest)
       (types ...)
       rest?
       call-and-check-return-type/multiple
       (list ret1? ret2? ret*? ...)
       body ...))
    ((_ (procname args ... . rest)
        (types ... (:: rest1? rest2? rest*? ...) (-> ret1? ret2? ret*? ...))
        body ...)
     (define-typed/helper procname (args ... . rest)
       (types ...)
       (circular-rest (list rest1? rest2? rest*? ...))
       call-and-check-return-type/multiple
       (list ret1? ret2? ret*? ...)
       body ...))    ;; single return checker: only check one value, further values are
    ;; discarded except if ret? is #f: then do not check, keep all
    ;; values
    ;; Compat for return type delimited by ->: if there is one arg
    ;; more than types, then ret? is treated as type.
    ((_ (procname args ...) (ret? types ...)
        body ...)
     (define-typed/compat define-typed/helper (procname args ...)
       (ret? types ...)
       body ...))
    ))
    ;; TODO add blocks for rest support without ret? Do not support
    ;; ret? types ... :: rest? for now, that’s already provided by
    ;; :: rest? ⇒ (types ... :: rest?) need not check for
    ;; a return type as long as rest? does not support ret? t ...

(define-syntax lambda-typed
  (syntax-rules ()
    ((_ (args ...) body ...)
     (define-typed (#f args ...) body ...))
    ((_ (args ... . rest) body ...)
     (define-typed (#f args ... . rest) body ...))))


;; specific to define-typed*
(define-syntax check-types*
  (syntax-rules ()
    ((_ (#f types? ...) (argument arguments ...)) ;; no type check for argument
     (check-types* (types? ...) (arguments ...)))
    ;; special handling for keyword arguments with default value.
    ((_ (type? types? ...) ((keyword-name default) arguments ...))
     (check-types* (type? types? ...) (keyword-name arguments ...)))
    ((_ (type? types? ...) (argument arguments ...))
     (begin
       (if (and (keyword? type?)
                (keyword? argument))
           (unless (equal? type? argument)
             (error "Keywords in arguments and types differ ~a ~a"
                    type? argument))
           (unless (type? argument)
             (error "type error ~a ~a" type? argument)))
       (check-types* (types? ...) (arguments ...))))
    ((_ () (arguments ...)) #f))) ;; untyped arguments are legal when there’s a return type

;; helper with keyword support
(define-syntax-rule (define-typed*/helper procname
                      (args ...) (types ...)
                      rest? ret-proc ret-values
                      body ...)
  (define-typed/base procname
    (args ...) (types ...)
    rest? ret-proc ret-values
    define* lambda* check-types* ;; with keywords
    body ...))

;; Define a procedure with typecheck, taking keywords into acount like
;; define*.
(define-syntax define-typed*
  (syntax-rules (->)
    ;; syntax with -> ret
    ;; single -> checker: check all returned values via procedure
    ((_ (procname args ...) (types ... (-> ret?))
        body ...)
     (define-typed* (procname args ...)
       ((-> ret?) types ...)
        body ...))
    ;; two or more return checkers: one per value (fixed number of
    ;; return values!)
    ((_ (procname args ...) (types ... (-> ret1? ret2* ret*? ...))
        body ...)
     (define-typed* (procname args ...)
       ((-> ret1? ret2* ret*? ...) types ...)
       body ...))
    ;; alternate single return value syntax with -> ret
    ((_ (procname args ...) (types ... -> ret?)
        body ...)
     (define-typed*/helper procname (args ...) (types ...)
       #f
       call-and-check-return-type
       ret?
       body ...))
    ;; single checker: check all returned values via procedure
    ((_ (procname args ...)
        ((-> ret?) types ...)
        body ...)
     (define-typed*/helper procname (args ...) (types ...)
       #f
       call-and-check-return-type/proc
       ret?
       body ...))
    ;; two or more return checkers: one per value (fixed number of
    ;; return values!)
    ((_ (procname args ...)
        ((-> ret1? ret2* ret*? ...) types ...)
        body ...)
     (define-typed*/helper procname (args ...)
       (types ...)
       #f
       call-and-check-return-type/multiple
       (list ret1? ret2* ret*? ...) ;; return-checkers
       body ...))
    ;; single return checker: only check one value, further values are
    ;; discarded except if ret? is #f: then do not check, keep all
    ;; values
    ((_ (procname args ...) (ret? types ...)
        body ...)
     (define-typed/compat define-typed*/helper (procname args ...)
       (ret? types ...)
       body ...))))

(define-syntax-rule (lambda-typed* (args ...) body ...)
  (define-typed* (#f args ...) body ...))
