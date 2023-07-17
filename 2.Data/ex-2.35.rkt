#lang scheme

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

;(define (count-leaves tree)
;  (accumulate (lambda (sub-tree result)
;                (if (pair? sub-tree)
;                    (+ (count-leaves sub-tree)
;                       result)
;                    (+ 1
;                       result)))
;              0
;              tree))

(define (count-leaves tree)
  (accumulate + 0 (map (lambda (sub-tree)
                         (cond ((null? sub-tree) 0)
                               ((not (pair? sub-tree)) 1)
                               (else (count-leaves sub-tree))))
                       tree)))

; Testing
(define test-tree-1 null)
(define test-tree-2 (list 1 2 3 4 5 6 7 8))
(define test-tree-3 (list 1 2 (list 3 (list 4) (list 5 6)) 7 (list 8 9)))

(count-leaves test-tree-1) ; 0
(count-leaves test-tree-2) ; 8
(count-leaves test-tree-3) ; 9
