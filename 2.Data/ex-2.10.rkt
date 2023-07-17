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

; Public methods
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-intervals x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Division error (interval spans 0)")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

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
(define i (make-interval -2 10))
(define j (make-interval 8 12))
(define k (make-interval 4 10))

(print-name-interval "i" i) ; i: [-2, 10]
(print-name-interval "j" j) ; j: [8, 12]
(print-name-interval "k" k) ; k: [4, 10]
 
(print-name-interval "j/k" (div-interval j k)) ; j/k: [0.8, 3.0]
(print-name-interval "j/i" (div-interval j i)) ; error!
