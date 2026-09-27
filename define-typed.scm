; SPDX-FileCopyrightText: 2026 Arne Babenhauserheide
;
; SPDX-License-Identifier: MIT

(define-module (define-typed) #:export (define-typed lambda-typed define-typed* lambda-typed*))

(import (srfi :11 let-values))

;; common procedures
(define-inlinable (takes-single-value? proc)
  (equal? '(1 0 #f) (procedure-minimum-arity proc)))

(define (call-and-check-return-type proc ret?)
  (if ret? ;; #f means: do not check
      ;; get the result
      (let ((res (proc)))
        ;; typecheck the result
        ;; TODO report a bug in Guile that this causes trouble with => in cond when using define-inlinable
        (unless (ret? res)
          (error "type error: return value ~a does not match ~a"
                 res ret?))
        ;; return the result
        res)
      (proc)))

(define
  (call-and-check-return-type/proc proc check-values)
  ;; get the result
  (let-values ((res (proc)))
    ;; typecheck the result
    (unless (check-values res)
      (error "type error: return values ~a do not match ~a"
             res check-values))
    ;; return the result
    (apply values res)))
(define
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

(define (check-argument-and-type-count args types)
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

(define (add-properties! proc name from-proc ret? types)
  ;; add procedure properties via an inner procedure
  (set-procedure-properties! proc (procedure-properties from-proc))
  ;; record the types
  (set-procedure-property! proc 'return-types ret?)
  (set-procedure-property! proc 'argument-types types)
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


(define-syntax-rule (typed/base
                      (args ...) (types ...)
                      ret-proc ret-values
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

(define-syntax define-typed/base
  (syntax-rules ()
    ((_ #f
        (args ...) (types ...)
        ret-proc ret-values
        def lamb check ;; define or define*, ...
        body ...)
     (begin
       (let ((proc
              (typed/base
               (args ...) (types ...)
               ret-proc ret-values
               def lamb check ;; define or define*, ...
               body ...))
             (properties-helper (lamb (args ...) body ...)))
         ;; add properties to the defined procedure
         (add-properties! proc #f properties-helper
                          ret-values (list types ...))
         proc)))
    ((_ procname
        (args ...) (types ...)
        ret-proc ret-values
        def lamb check ;; define or define*, ...
        body ...)
     (begin
       (def procname
            (typed/base
             (args ...) (types ...)
             ret-proc ret-values
             def lamb check ;; define or define*, ...
             body ...))
       (let ((properties-helper (lamb (args ...) body ...)))
         ;; add properties to the defined procedure
         (add-properties! procname (and procname 'procname) properties-helper
                          ret-values (list types ...)))))))

;; helper without keyword support
(define-syntax-rule (define-typed/helper procname
                      (args ...) (types ...)
                      ret-proc ret-values
                      body ...)
  (define-typed/base procname
    (args ...) (types ...)
    ret-proc ret-values
    define lambda check-types ;; without keywords
    body ...))

;; helper to distinguish between first type being ret? and first type
;; being first argument
(define-syntax define-typed/compat
  (syntax-rules (copied-> reversed->)
    ((_ helper (procname copied-> a ... reversed-> ())
        (ret? copied-> t ... reversed-> ())
        body ...)
     (helper procname (a ...) (t ...)
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



;; Define a procedure with typechecks.
(define-syntax define-typed
  (syntax-rules (->)
    ;; syntax with -> ret
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
    ;; alternate single return value syntax with -> ret
    ((_ (procname args ...)
        (types ... -> ret?)
        body ...)
     (define-typed/helper procname (args ...)
       (types ...)
       call-and-check-return-type
       ret?
       body ...))
    ;; single checker: check all returned values via procedure
    ((_ (procname args ...)
        ((-> ret?) types ...)
        body ...)
     (define-typed/helper procname (args ...)
       (types ...)
       call-and-check-return-type/proc
       ret?
       body ...))
    ;; two or more return checkers: one per value (fixed number of
    ;; return values!)
    ((_ (procname args ...)
        ((-> ret1? ret2* ret*? ...) types ...)
        body ...)
     (define-typed/helper procname (args ...)
       (types ...)
       call-and-check-return-type/multiple
       (list ret1? ret2* ret*? ...) ;; return-checkers
       body ...))
    ;; alternate single return syntax with -> ret
    ((_ (procname args ...)
        (types ... -> ret?)
        body ...)
     (define-typed/helper procname (args ...)
       (types ...)
       call-and-check-return-type
       ret?
       body ...))
    ;; single return checker: only check one value, further values are
    ;; discarded except if ret? is #f: then do not check, keep all
    ;; values
    ;; Compat for return type delimited by ->: if there is one arg
    ;; more than types, then ret? is treated as type.
    ((_ (procname args ...) (ret? types ...)
        body ...)
     (define-typed/compat define-typed/helper (procname args ...)
       (ret? types ...)
       body ...))))

(define-syntax-rule (lambda-typed (args ...) body ...)
  (define-typed (#f args ...) body ...))


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
                      ret-proc ret-values
                      body ...)
  (define-typed/base procname
    (args ...) (types ...)
    ret-proc ret-values
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
       call-and-check-return-type
       ret?
       body ...))
    ;; single checker: check all returned values via procedure
    ((_ (procname args ...)
        ((-> ret?) types ...)
        body ...)
     (define-typed*/helper procname (args ...) (types ...)
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
