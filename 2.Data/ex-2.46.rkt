#lang sicp
(#%require sicp-pict)

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

(define v (make-vect 5 6))
(define u (make-vect 3 2))
(define s 2)

(add-vect v u)
(sub-vect v u)
(scale-vect s v)
