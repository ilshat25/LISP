#lang racket

; Constructor
(define make-interval cons)

; Selectors
; lower-bound gives smallest border
(define (lower-bound interval)
  (min (car interval) (cdr interval)))

; upper-bound gives higher border
(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (make-center-percent c prc)
  (let ((w (* c (/ prc 100))))
    (make-interval (- c w) (+ c w))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (* (/ (width i) (center i)) 100))

; Helper functions
(define (print-interval i)
  (display "[")
  (display (lower-bound i))
  (display ", ")
  (display (upper-bound i))
  (display "]"))

(define (print-name-interval name i)
  (display name)
  (display ": ")
  (print-interval i)
  (newline))

; Testing
(define i (make-center-percent 10 20))
(print-name-interval "i" i)
(display "center: ")
(display (center i))
(newline)
(display "width: ")
(display (width i))
(newline)
(display "percent: ")
(display (percent i))
(display "%")
(newline)
