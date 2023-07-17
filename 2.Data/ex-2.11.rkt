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

(define (mul-interval-new x y)
  ; in this case we assume zero as positive
  (define (pos? n) (>= n 0))
  (define (neg? n) (< n 0))
  (let ((x0 (lower-bound x))
        (x1 (upper-bound x))
        (y0 (lower-bound y))
        (y1 (upper-bound y)))
    (cond
      ; 1: ----
      ((and (neg? x0) (neg? x1) (neg? y0) (neg? y1))
       (make-interval (* x0 y0) (* x1 y1)))
      ; 2: ---+
      ((and (neg? x0) (neg? x1) (neg? y0) (pos? y1))
       (make-interval (* x0 y1) (* x0 y0)))
      ; 3: --++
      ((and (neg? x0) (neg? x1) (pos? y0) (pos? y1))
       (make-interval (* x0 y1) (* x1 y0)))
      ; 4: -+--
      ((and (neg? x0) (pos? x1) (neg? y0) (neg? y1))
       (make-interval (* x1 y1) (* x0 y1)))
      ; 5: -+-+
      ((and (neg? x0) (pos? x1) (neg? y0) (pos? y1))
       (make-interval (min (* x0 y1) (* x1 y0))
                      (max (* x0 y0) (* x1 y1))))
      ; 6: -+++
      ((and (neg? x0) (pos? x1) (pos? y0) (pos? y1))
       (make-interval (* x0 y1) (* x1 y1)))
      ; 7: ++--
      ((and (pos? x0) (pos? x1) (neg? y0) (neg? y1))
       (make-interval (* x1 y0) (* x0 y1)))
      ; 8: ++-+
      ((and (pos? x0) (pos? x1) (neg? y0) (pos? y1))
       (make-interval (* x1 y0) (* x1 y1)))
      ; 9: ++++
      ((and (pos? x0) (pos? x1) (pos? y0) (pos? y1))
       (make-interval (* x0 y0) (* x1 y1)))
      ; error
      (else (error "There's an error in interval selectors (lower-bound > upper-bound)."))
      )))

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Division error (interval spans 0)")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

; Helper functions
(define (display-interval i)
  (display "[")
  (display (lower-bound i))
  (display ", ")
  (display (upper-bound i))
  (display "]"))

(define (equal-interval a b)
  (and (= (lower-bound a) (lower-bound b))
       (= (upper-bound a) (upper-bound b))))

(define (display-name-interval name i)
  (display name)
  (display ": ")
  (display-interval i)
  (newline))

; Compare two mul functions
(define (ensure-mult-works x0 x1 y0 y1)
  (define (display-ok a b r1 r2)
    (display "Ok ... ")
    (display-interval a)
    (display " * ")
    (display-interval b)
    (display ": ")
    (display-interval r1)
    (display " == ")
    (display-interval r2)
    (newline))
  (define (display-error a b r1 r2)
    (display "Error ... ")
    (display-interval a)
    (display " * ")
    (display-interval b)
    (display ": ")
    (display-interval r1)
    (display " != ")
    (display-interval r2)
    (newline))
  (let ((a (make-interval x0 x1))
        (b (make-interval y0 y1)))
    (let ((r1 (mul-interval a b))
          (r2 (mul-interval-new a b)))
      (if (equal-interval r1 r2)
        (display-ok a b r1 r2)
        (display-error a b r1 r2)))))

; Testing

(ensure-mult-works  +10 +10   +10 +10) 
 (ensure-mult-works  +10 +10   +00 +10) 
 (ensure-mult-works  +10 +10   +00 +00) 
 (ensure-mult-works  +10 +10   +10 -10) 
 (ensure-mult-works  +10 +10   -10 +00) 
 (ensure-mult-works  +10 +10   -10 -10) 
  
 (ensure-mult-works  +00 +10   +10 +10) 
 (ensure-mult-works  +00 +10   +00 +10) 
 (ensure-mult-works  +00 +10   +00 +00) 
 (ensure-mult-works  +00 +10   +10 -10) 
 (ensure-mult-works  +00 +10   -10 +00) 
 (ensure-mult-works  +00 +10   -10 -10) 
  
 (ensure-mult-works  +00 +00   +10 +10) 
 (ensure-mult-works  +00 +00   +00 +10) 
 (ensure-mult-works  +00 +00   +00 +00) 
 (ensure-mult-works  +00 +00   +10 -10) 
 (ensure-mult-works  +00 +00   -10 +00) 
 (ensure-mult-works  +00 +00   -10 -10) 
  
 (ensure-mult-works  +10 -10   +10 +10) 
 (ensure-mult-works  +10 -10   +00 +10) 
 (ensure-mult-works  +10 -10   +00 +00) 
 (ensure-mult-works  +10 -10   +10 -10) 
 (ensure-mult-works  +10 -10   -10 +00) 
 (ensure-mult-works  +10 -10   -10 -10) 
  
 (ensure-mult-works  -10 +00   +10 +10) 
 (ensure-mult-works  -10 +00   +00 +10) 
 (ensure-mult-works  -10 +00   +00 +00) 
 (ensure-mult-works  -10 +00   +10 -10) 
 (ensure-mult-works  -10 +00   -10 +00) 
 (ensure-mult-works  -10 +00   -10 -10) 
  
 (ensure-mult-works  -10 -10   +10 +10) 
 (ensure-mult-works  -10 -10   +00 +10) 
 (ensure-mult-works  -10 -10   +00 +00) 
 (ensure-mult-works  -10 -10   +10 -10) 
 (ensure-mult-works  -10 -10   -10 +00) 
 (ensure-mult-works  -10 -10   -10 -10)

; All Ok!