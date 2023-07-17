#lang sicp

(define (make-monitored func)
  (let ((count 0))
    (define (how-many-calls?) count)
    (define (reset-count) (set! count 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) (how-many-calls?))
            ((eq? m 'reset-count) (reset-count))
            (else
             (begin (set! count (+ count 1))
                    (func m)))))
    dispatch))

;;; Test
(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)
(s 200)
(s 10)
(s 'how-many-calls?)
