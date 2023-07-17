#lang scheme

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (list x))) null sequence))

(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))

; Testing

(define test-data-1 null)
(define test-data-2 (list 1 2 3 4))
(define test-data-3 (list 1 2 3 4 5 6 7))

(reverse-right test-data-1) ; ()
(reverse-left test-data-1)  ; ()

(reverse-right test-data-2) ; (4 3 2 1)
(reverse-left test-data-2)  ; (4 3 2 1)

(reverse-right test-data-3) ; (7 6 5 4 3 2 1)
(reverse-left test-data-3)  ; (7 6 5 4 3 2 1)
