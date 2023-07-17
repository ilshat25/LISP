#lang scheme

(define (square x) (* x x))

(define (square-tree-1 tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-1 (car tree))
                    (square-tree-1 (cdr tree))))))

(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-2 sub-tree)
             (square sub-tree)))
       tree))

; Testing
(define test-tree-1 null)
(define test-tree-2 (list 1))
(define test-tree-3 (list 1 2 3))
(define test-tree-4 (list (list 1 2 3) 4 (list (list 5 6) (list 7 8))))

(square-tree-1 test-tree-1) ; ()
(square-tree-1 test-tree-2) ; (1)
(square-tree-1 test-tree-3) ; (1 4 9)
(square-tree-1 test-tree-4) ; ((1 4 9) 16 ((25 36) (49 64)))

(square-tree-2 test-tree-1) ; ()
(square-tree-2 test-tree-2) ; (1)
(square-tree-2 test-tree-3) ; (1 4 9)
(square-tree-2 test-tree-4) ; ((1 4 9) 16 ((25 36) (49 64)))
