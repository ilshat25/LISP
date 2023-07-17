#lang sicp

; Tree abstraction
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

; O(nlogn) realisation (because of "append" O(n) complexity)
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

; O(n) realisation using "cons" instead of "append"
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; Tests
(define tree-1 (make-tree 7
                          (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
                          (make-tree 9 '() (make-tree 11 '() '()))))
(define tree-2 (make-tree 3
                          (make-tree 1 '() '())
                          (make-tree 7 (make-tree 5 '() '()) (make-tree 9 '() (make-tree 11 '() '())))))
(define tree-3 (make-tree 5
                          (make-tree 3 (make-tree 1 '() '()) '())
                          (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '()))))

; Both functions have the same in-order result
(tree->list-1 tree-1)
(tree->list-2 tree-1)
(tree->list-1 tree-2)
(tree->list-2 tree-2)
(tree->list-1 tree-3)
(tree->list-2 tree-3)
