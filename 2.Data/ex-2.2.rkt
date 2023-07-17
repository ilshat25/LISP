#lang racket

; Point presentation
(define make-point cons)
(define x-point car)
(define y-point cdr)

; Segment presentation
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

; Returns middle point of segment
(define (midpoint-segment seg)
  (make-point (/ (+ (x-point (start-segment seg))
                    (x-point (end-segment seg)))
                 2.0)
              (/ (+ (y-point (start-segment seg))
                    (y-point (end-segment seg)))
                 2.0)))

; Helper functions
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

; Testing
(define p-1 (make-point 2 2))
(define p-2 (make-point 5 9))
(define seg (make-segment p-1 p-2))

(print-point p-1)
(print-point p-2)
(print-point (midpoint-segment seg))