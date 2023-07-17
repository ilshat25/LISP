#lang scheme

(define (same-parity x . z)
  (let ((x-remainder (remainder x 2)))
    (define (same-parity-iter z)
      (cond ((null? z) null)
            ((= x-remainder (remainder (car z) 2))
             (cons (car z) (same-parity-iter (cdr z))))
            (else (same-parity-iter (cdr z)))))
    (cons x (same-parity-iter z))))

; Testing

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7 8)