#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        ((> x (car set)) (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  ( if (or (null? set1) (null? set2))
       '()
       (let ((x1 (car set1))
             (x2 (car set2)))
         (cond ((= x1 x2)
                (cons x1 (intersection-set (cdr set1) (cdr set2))))
               ((< x1 x2) (intersection-set (cdr set1) set2))
               ((> x1 x2) (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ((first (car set))
            (others (cdr set)))
        (cond ((> x first)
               (cons first
                     (adjoin-set x others)))
              ((= x first) set)
              ((< x first)
               (cons x set))))))

; Tests

(define set1 '(1 2 3 4 5))
(define set2 '(3 4 5 6 7))

(element-of-set? 1 set1)
(element-of-set? 1 set2)

(intersection-set set1 set2)

(adjoin-set 0 set1)
(adjoin-set 1 set1)
(adjoin-set 3.5 set1)
(adjoin-set 6 set1)
