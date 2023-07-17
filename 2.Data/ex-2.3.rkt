#lang racket

; Point presentation
(define make-point cons)
(define x-point car)
(define y-point cdr)

; Segment presentation
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

; Rectangle 1st presenation by two points
;(define make-rect cons)
;(define rect-bottom-left car)
;(define rect-top-right cdr)

;(define (rect-width rect)
;  (let ((x-1 (x-point (rect-bottom-left rect)))
;        (x-2 (x-point (rect-top-right rect))))
;    (abs (- x-1 x-2))))

;(define (rect-height rect)
;  (let ((y-1 (y-point (rect-bottom-left rect)))
;        (y-2 (y-point (rect-top-right rect))))
;    (abs (- y-1 y-2))))

; Rectangle 2d presentation by bottom left point, length and width;
(define (make-rect bottom-left width height)
  (cons bottom-left (cons width height)))
(define rect-bottom-left car)
(define (rect-top-right react)
  (let ((x (x-point (rect-bottom-left)))
        (y (y-point (rect-bottom-left))))
    (make-point (+ x (rect-width rect))
                (+ y (rect-height rect)))))

(define (rect-width rect) (car (cdr rect)))
(define (rect-height rect) (cdr (cdr rect)))

; Public methods
(define (rect-area rect)
  (* (rect-width rect) (rect-height rect)))
(define (rect-perimeter rect)
  (* 2 (+ (rect-width rect) (rect-height rect))))

; Testing
(define rect
  (make-rect (make-point 2 2) 6 6))
(rect-area rect)
(rect-perimeter rect)