#lang scheme

(define (tree-map tree proc)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map sub-tree proc)
             (proc sub-tree)))
       tree))

; Testing
(define test-tree-1 null)
(define test-tree-2 (list 1))
(define test-tree-3 (list 1 2 3))
(define test-tree-4 (list (list 1 2 3) 4 (list (list 5 6) (list 7 8))))

(define (factor-tree tree factor)
  (tree-map tree (lambda (x) (* x factor))))

(define (square x) (* x x))
(define (square-tree tree) (tree-map tree square))

(factor-tree test-tree-1 10) ; ()
(factor-tree test-tree-2 10) ; (10)
(factor-tree test-tree-3 10) ; (10 20 30)
(factor-tree test-tree-4 10) ; ((10 20 30) 40 ((50 60) (70 80)))

(square-tree test-tree-1)    ; ()
(square-tree test-tree-2)    ; (1)
(square-tree test-tree-3)    ; (1 4 9)
(square-tree test-tree-4)    ; ((1 4 9) 16 ((25 36) (49 64)))
