#lang racket

; list-ref returns n-th element of list
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

; length returns the length of the list
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

; length in iterative style
;(define (length items)
;  (define (length-iter a count)
;    (if (null? a)
;        count
;        (length-iter (cdr a) (+ 1 count))))
;  (length-iter items 0))

; append adds list1 to list2
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

; Testing
(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7))

(list-ref squares 3)
(length odds)
(append squares odds)
