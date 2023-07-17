#lang scheme

;(define (fringe items)
;  (cond ((null? items) null)
;        ((pair? items)
;         (append (fringe (car items))
;                 (fringe (cdr items))))
;        (else (list items))))

(define (fringe items)
  (cond ((null? items) null)
        ((pair? (car items))
         (append (fringe (car items))
                      (fringe (cdr items))))
        (else (cons (car items) (fringe (cdr items))))))

; Testing

(define test-list-1 null)
(define test-list-2 (list 1))
(define test-list-3 (list 1 2 3 4 5))
(define test-list-4 (list (list 1 2) 3 4))
(define test-list-5 (list (list 1 2) (list (list 3 4) (list 5 6))))

(fringe test-list-1)
(fringe test-list-2)
(fringe test-list-3)
(fringe test-list-4)
(fringe test-list-5)
