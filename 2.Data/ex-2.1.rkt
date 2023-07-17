#lang racket
; Presentaion of ration numbers
; and operations on it using lisp pair system

; Constructor
(define (make-rat n d)
  (if (< d 0)
      (cons (- n) (- d))
      (cons n d)))
; Selectors
(define (numer x) (car x))
(define (denom x) (cdr x))

; Operations
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

; Help functions
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))

; Example
(define one-half (make-rat -1 -2))
(print-rat one-half)
; 1/2
(define one-third (make-rat 1 -3))
(print-rat (add-rat one-half one-third))
; 5/6
(print-rat (mul-rat one-half one-third))
; 1/6
(print-rat (add-rat one-third one-third))
; 2/3
