#!/usr/bin/env bash
# -*- mode: scheme -*-
# SPDX-FileCopyrightText: 2026 Arne Babenhauserheide
#
# SPDX-License-Identifier: MIT
exec guile -L . "$0"
; !#

(import (define-typed) (statprof))

(define-inlinable (float? x)
  (and (real? x) (inexact? x)))
(define-inlinable (all-float? args)
  (not (member #f (map float? args))))

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
    magnitude-typed/no-return-check-by-missing-type
    magnitude-typed/return
    magnitude-typed/return-multiple
    magnitude-typed/return-proc
    magnitude-typed/return-lambda
    magnitude-typed/return->
    magnitude-typed/only-return->
    magnitude-typed/return-multiple->
    magnitude-typed/return-proc->
    magnitude-typed/return-lambda->
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
