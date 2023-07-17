#lang scheme

(define (for-each proc items)
  (if (null? items)
      #t
      (and (proc (car items))
           (for-each proc (cdr items)))))

; Testing

(define test-list-1 null)
(define test-list-2 (list 1))
(define test-list-3 (list 57 321 88))

(define (display-list items)
  (for-each (lambda (x) (newline) (display x))
            items))

(display-list test-list-1)
(display-list test-list-2)
(display-list test-list-3)
