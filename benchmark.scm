#!/usr/bin/env bash
# -*- mode: scheme -*-
# SPDX-FileCopyrightText: 2026 Arne Babenhauserheide
#
# SPDX-License-Identifier: MIT
exec guile -L . "$0"
; !#

(import (define-typed) (statprof) (srfi :26))

(define-inlinable (float? x)
  (and (real? x) (inexact? x)))
(define-inlinable (all-float? args)
  (let loop ((args args))
    (cond ((null? args) #t)
          ((float? (car args))
           (loop (cdr args)))
          (else #f))))

(define (magnitude x y) (sqrt (+ (* x x) (* y y))))
(define (magnitude-handtyped x y)
  (unless (and (float? x) (float? y))
    (error "expected floats" x y))
  (sqrt (+ (* x x) (* y y))))

(define-typed
  (magnitude-typed/no-return-check x y)
  (#f float? float?)
  (sqrt (+ (* x x) (* y y))))

(define-typed
  (magnitude-typed/no-return-check-by-missing-type x y)
  (float? float?)
  (sqrt (+ (* x x) (* y y))))

(define-typed
  (magnitude-typed/return x y)
  (float? float? float?)
  (sqrt (+ (* x x) (* y y))))

(define-typed
  (magnitude-typed/rest-each-no-return a b . c) (float? float? :: float?)
  (sqrt (+ (* a a) (* b b))))

(define-typed
  (magnitude-typed/rest-one-for-all-no-return a b . c) (float? float? (:: all-float?))
  (sqrt (+ (* a a) (* b b))))

(define-typed
  (magnitude-typed/rest-two-alternating-no-return a b . c) (float? float? (:: float? string?))
  (sqrt (+ (* a a) (* b b))))

(define-typed
  (magnitude-typed/rest-each-untyped-arg-no-return a . c) (:: float?)
  (sqrt (apply + c)))

(define-typed
  (magnitude-typed/rest-each-only-no-return . c) (:: float?)
  (sqrt (apply + c)))

(define-typed
  (magnitude-typed/rest-one-for-all-only-no-return . c) ((:: all-float?))
  (sqrt (apply + c)))

(define-typed ;; alternating rest, but the same predicate, to match the call
  (magnitude-typed/rest-two-alternating-only-no-return . c) ((:: float? float?))
  (sqrt (apply + c)))

(define-typed
  (magnitude-typed/rest-each a b . c) (float? float? :: float? -> float?)
  (sqrt (+ (* a a) (* b b))))

(define-typed
  (magnitude-typed/rest-one-for-all a b . c) (float? float? (:: all-float?) -> float?)
  (sqrt (+ (* a a) (* b b))))

(define-typed
  (magnitude-typed/rest-two-alternating a b . c) (float? float? (:: float? string?) -> float?)
  (sqrt (+ (* a a) (* b b))))

(define-typed
  (magnitude-typed/rest-each-ret-proc-> a b . c) (float? float? :: float? (-> all-float?))
  (sqrt (+ (* a a) (* b b))))

(define-typed
  (magnitude-typed/rest-one-for-all-ret-proc-> a b . c) (float? float? (:: all-float?) (-> all-float?))
  (sqrt (+ (* a a) (* b b))))

(define-typed
  (magnitude-typed/rest-two-alternating-ret-proc-> a b . c) (float? float? (:: float? string?) (-> all-float?))
  (sqrt (+ (* a a) (* b b))))

(define-typed
  (magnitude-typed/rest-each-ret-multiple-> a b . c) (float? float? :: float? (-> float? float?))
  (values 1.0 (sqrt (+ (* a a) (* b b)))))

(define-typed
  (magnitude-typed/rest-one-for-all-ret-multiple-> a b . c) (float? float? (:: all-float?) (-> float? float?))
  (values 1.0 (sqrt (+ (* a a) (* b b)))))

(define-typed
  (magnitude-typed/rest-two-alternating-ret-multiple-> a b . c) (float? float? (:: float? string?) (-> float? float?))
  (values 1.0 (sqrt (+ (* a a) (* b b)))))

;; number range contract with rest arguments

(define-typed
  (magnitude-typed/number-range-contract a . b) ((cut = 3.0 <>) (:: (cut < 3.5 <> 5.5) string?) (-> (cut < 0.9 <> 1.1) float?))
  (values 1.0 (sqrt (+ (* a a) (* (car b) (car b))))))

(magnitude-typed/number-range-contract 3.0 4.0 "a" 4.2 "b")

;; return types

(define-typed
  (magnitude-typed/return-multiple x y)
  ((-> float? float?) float? float?)
  (values 1.0 (sqrt (+ (* x x) (* y y)))))

(define-typed
  (magnitude-typed/return-proc x y)
  ((-> all-float?) float? float?)
  (values 1.0 (sqrt (+ (* x x) (* y y)))))

(define-typed
  (magnitude-typed/return-lambda x y)
  ((-> (λ (vals) (apply > vals))) number? number?)
  (values (sqrt (+ (* x x) (* y y))) x))

(define-typed
  (magnitude-typed/only-return-> x y)
  (-> float?)
  (sqrt (+ (* x x) (* y y))))

(define-typed
  (magnitude-typed/return-> x y)
  (float? float? -> float?)
  (sqrt (+ (* x x) (* y y))))

(define-typed
  (magnitude-typed/return->lambda x y)
  (float? float? -> (λ (x) (float? x)))
  (sqrt (+ (* x x) (* y y))))

(define-typed
  (magnitude-typed/return-multiple-> x y)
  (float? float? (-> float? float?))
  (values 1.0 (sqrt (+ (* x x) (* y y)))))

(define-typed
  (magnitude-typed/return-multiple->lambda x y)
  (float? float? (-> (λ (x) (float? x)) float?))
  (values 1.0 (sqrt (+ (* x x) (* y y)))))

(define-typed
  (magnitude-typed/return-proc-> x y)
  (float? float? (-> all-float?))
  (values 1.0 (sqrt (+ (* x x) (* y y)))))

(define-typed
  (magnitude-typed/return-proc->lambda x y)
  (float? float? (-> (λ (x) (all-float? x))))
  (values 1.0 (sqrt (+ (* x x) (* y y)))))

(define-typed
  (magnitude-typed/return-lambda-> x y)
  (number? number? (-> (λ (vals) (apply > vals))))
  (values (sqrt (+ (* x x) (* y y))) x))

(define-typed*
  (magnitude-typed*/no-return-check x y #:key foo)
  (#f float? float? #:key not)
  (sqrt (+ (* x x) (* y y))))

(define-typed*
  (magnitude-typed*/no-return-check-by-missing-type x y #:key foo)
  (float? float? #:key not)
  (sqrt (+ (* x x) (* y y))))

(define-typed*
  (magnitude-typed*/return-keyword-with-default x y #:key (foo #t))
  (float? float? float? #:key identity)
  (sqrt (+ (* x x) (* y y))))

(define-typed*
  (magnitude-typed*/return-rest x y #:rest r)
  (float? float? float? #:rest null?)
  (sqrt (+ (* x x) (* y y))))

(define-typed*
  (magnitude-typed*/return x y #:key foo)
  (float? float? float? #:key not)
  (sqrt (+ (* x x) (* y y))))

(define-typed*
  (magnitude-typed*/return-multiple x y #:key foo)
  ((-> float? float?) float? float? #:key not)
  (values 1.0 (sqrt (+ (* x x) (* y y)))))

(define-typed*
  (magnitude-typed*/return-proc x y #:key foo)
  ((-> all-float?) float? float? #:key not)
  (values 1.0 (sqrt (+ (* x x) (* y y)))))

(define-typed*
  (magnitude-typed*/return-> x y #:key foo)
  (float? float? #:key not -> float?)
  (sqrt (+ (* x x) (* y y))))

(define-typed*
  (magnitude-typed*/return->keyword-with-default x y #:key (foo #t))
  (float? float? #:key identity -> float?)
  (sqrt (+ (* x x) (* y y))))

(define-typed*
  (magnitude-typed*/return->rest x y #:rest r)
  (float? float? #:rest null? -> float?)
  (sqrt (+ (* x x) (* y y))))

(define-typed*
  (magnitude-typed*/return-multiple-> x y #:key foo)
  (float? float? #:key not (-> float? float?))
  (values 1.0 (sqrt (+ (* x x) (* y y)))))

(define-typed*
  (magnitude-typed*/return-proc-> x y #:key foo)
  (float? float? #:key not (-> all-float?))
  (values 1.0 (sqrt (+ (* x x) (* y y)))))

(define-typed*
  (magnitude-typed*/return-lambda-> x y)
  (number? number? (-> (λ (vals) (apply > vals))))
  (values (sqrt (+ (* x x) (* y y))) x))

(define (benchmark proc)
  (display proc)(newline)
  (statprof
    (λ _
      (let lp ((i 0))
        (when (< i 20000000)
          (proc 3.0 4.0)
          (lp (+ i 1)))))))

(for-each benchmark
  (list
    magnitude
    magnitude-handtyped
    magnitude-typed/no-return-check
    magnitude-typed/return
    ;; rest argument examples
    magnitude-typed/rest-each-no-return
    magnitude-typed/rest-one-for-all-no-return
    magnitude-typed/rest-two-alternating-no-return
    magnitude-typed/rest-each
    magnitude-typed/rest-one-for-all
    magnitude-typed/rest-two-alternating
    magnitude-typed/rest-each-untyped-arg-no-return
    magnitude-typed/rest-each-only-no-return
    magnitude-typed/rest-one-for-all-only-no-return
    magnitude-typed/rest-two-alternating-only-no-return
    magnitude-typed/rest-each-ret-proc->
    magnitude-typed/rest-one-for-all-ret-proc->
    magnitude-typed/rest-two-alternating-ret-proc->
    magnitude-typed/rest-each-ret-multiple->
    magnitude-typed/rest-one-for-all-ret-multiple->
    magnitude-typed/rest-two-alternating-ret-multiple->
    ;; full-blown contracts
    magnitude-typed/number-range-contract
    ;; return check examples
    magnitude-typed/no-return-check-by-missing-type
    magnitude-typed/return-multiple
    magnitude-typed/return-proc
    magnitude-typed/return-lambda
    magnitude-typed/return->
    magnitude-typed/only-return->
    magnitude-typed/return-multiple->
    magnitude-typed/return-proc->
    magnitude-typed/return-lambda->
    ;; define-typed* examples
    magnitude-typed*/no-return-check
    magnitude-typed*/no-return-check-by-missing-type
    magnitude-typed*/return
    magnitude-typed*/return-keyword-with-default
    magnitude-typed*/return-rest
    magnitude-typed*/return-multiple
    magnitude-typed*/return-proc
    magnitude-typed*/return->
    magnitude-typed*/return->keyword-with-default
    magnitude-typed*/return->rest
    magnitude-typed*/return-multiple->
    magnitude-typed*/return-proc->))
