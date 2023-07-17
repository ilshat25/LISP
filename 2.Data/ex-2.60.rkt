#lang sicp

; Unordered set with repetitions.
; Useful when need to do a lot of adjoin and union operations
; but it costs addition memory.
; - element-of-set?  -> O(n);
; - adjoin-set       -> O(1);
; - intersection-set -> O(n^2);
; - union-set        -> O(n).


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (intersection-set set1 set)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2) (append set1 set2))

; Test
(define set1 '(1 2 3 4))
(define set2 '(3 4 5 6))

(element-of-set? 1 set1)
(element-of-set? 1 set2)

(adjoin-set 5 set1)

(intersection-set set1 set2)
(union-set set1 set2)
