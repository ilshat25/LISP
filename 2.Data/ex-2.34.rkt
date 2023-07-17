#lang scheme

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

; Testing
(horner-eval 2 (list 1 3 0 5 0 1)) ; 79
(horner-eval 2 (list 1 3 0 5 0 2)) ; 111
