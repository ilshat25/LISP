#lang sicp

(define (square x) (* x x))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x)
                                        (square y))))
          ((eq? op 'angel) (atan y x))))
  dispatch)

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angel) a)))
  dispatch)

(define (apply-generic op arg) (arg op))

(define zr (make-from-real-imag 10 10))
(define zp (make-from-mag-ang 10 10))

(apply-generic 'real-part zr)
(apply-generic 'imag-part zr)
(apply-generic 'magnitude zr)
(apply-generic 'angel zr)

(apply-generic 'real-part zp)
(apply-generic 'imag-part zp)
(apply-generic 'magnitude zp)
(apply-generic 'angel zp)
