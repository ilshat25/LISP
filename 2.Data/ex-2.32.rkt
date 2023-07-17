#lang scheme

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))

; Testing
(define test-set-1 null)
(define test-set-2 (list 1))
(define test-set-3 (list 1 2 3))

(subsets test-set-1) ; (())
(subsets test-set-2) ; (() (1))
(subsets test-set-3) ; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
