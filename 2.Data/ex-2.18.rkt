#lang scheme

; Iterative
(define (reverse-i lst)
  (define (reverse-iter lst result)
    (if (null? lst)
        result
        (reverse-iter (cdr lst)
                      (cons (car lst) result))))
  (reverse-iter lst null))

; Recursive
(define (reverse-r lst)
  (if (null? lst)
      null
      (append (reverse-r (cdr lst))
              (list (car lst)))))

; Testing

(define test-list-1 (list 0 1 2 3 4 5 6 7 8 9))
(define test-list-2 (list 0))
(define test-list-3 null)

(reverse-i test-list-1)
(reverse-i test-list-2)
(reverse-i test-list-3)

(reverse-r test-list-1)
(reverse-r test-list-2)
(reverse-r test-list-3)
