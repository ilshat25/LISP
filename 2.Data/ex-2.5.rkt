#lang racket

; Helper functions
(define (square x) (* x x))

(define (even? x) (= (remainder x 2) 0))

(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp b (/ n 2))))
        (else (* b (fast-exp b (- n 1))))))

; Pairs using arifmetic operations
(define (cons a b)
  (* (fast-exp 2 a)
     (fast-exp 3 b)))

; 2^a = z
; a*log(2) = log(z)
; a = log(z) / log(2)
;(define (car x)
;  (define (divisible-by-3? x)
;    (= (remainder x 3) 0))
;  (if (divisible-by-3? x)
;      (car (/ x 3))
;      (round (/ (log x) (log 2)))))

;(define (cdr x)
;  (define (divisible-by-2? x)
;    (= (remainder x 2) 0))
;  (if (divisible-by-2? x)
;      (cdr (/ x 2))
;      (round (/ (log x) (log 3)))))

(define (largest-power-of a z)
  (if (= (remainder z a) 0)
      (+ 1 (largest-power-of a (/ z a)))
      0))

(define (car z) (largest-power-of 2 z))
(define (cdr z) (largest-power-of 3 z))

; Testing
(define z (cons 4 5))

(car z)
(cdr z)
