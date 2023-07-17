#lang sicp

(define rand-init 137)
(define (rand-update x)
  (let ((a 241)
        (b 631)
        (m 7547))
    (remainder (+ (* x a) b) m)))
(define rand
  (let ((x rand-init))
  (lambda (m)
    (cond ((eq? m 'generate)
           (set! x (rand-update x))
           x)
          ((eq? m 'reset)
           (set! x rand-init))
          (else
           (error "Неизвестный вызов -- RAND" m))))))

;;; Test
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'reset)
(rand 'generate)
(rand 'generate)
(rand 'generate)
    