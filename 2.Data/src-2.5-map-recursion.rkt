#lang scheme

(define (scale-tree-1 tree factor)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree-1 (car tree) factor)
                    (scale-tree-1 (cdr tree) factor)))))

(define (scale-tree-2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-2 sub-tree factor)
             (* sub-tree factor)))
       tree))

; Testing
(define test-list (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(scale-tree-1 test-list 10)
(scale-tree-2 test-list 10)