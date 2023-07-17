#lang sicp
(#%require sicp-pict)

; Vector
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v u)
  (make-vect (+ (xcor-vect v) (xcor-vect u))
             (+ (ycor-vect v) (ycor-vect u))))

(define (sub-vect v u)
  (make-vect (- (xcor-vect v) (xcor-vect u))
             (- (ycor-vect v) (ycor-vect u))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

; Segment
(define (make-segment v u) (cons v u))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

; Test
(define v (make-vect 5 6))
(define u (make-vect 3 2))
(define seg (make-segment v u))

(start-segment seg)
(end-segment seg)