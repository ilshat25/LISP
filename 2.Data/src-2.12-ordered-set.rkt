#lang sicp

; Time complexity: O(n/2) == O(n).
(define (element-of-set? x set)
  (cond ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

; Time complexity: O(2n) == O(n).
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2) (intersection-set (cdr set1) set2))
              ((> x1 x2) (intersection-set set1 (cdr set2)))))))

; Tests

(define set1 '(1 2 3 4))
(define set2 '(3 4 5 6))

(element-of-set? 1 set1)
(element-of-set? 1 set2)

(intersection-set set1 set2)
