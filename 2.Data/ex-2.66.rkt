#lang sicp

; Tree

(define (make-tree entry left right)
  (list entry left right))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      false
      (let ((x (car set-of-records)))
        (cond ((= given-key x) (car set-of-records))
              ((< given-key x) (lookup given-key
                                       (left-branch set-of-records)))
              ((> given-key x) (lookup given-key
                                       (right-branch set-of-records)))))))

; Testing
(define tree-1 (make-tree 7
                          (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
                          (make-tree 9 '() (make-tree 11 '() '()))))
(define tree-2 (make-tree 3
                          (make-tree 1 '() '())
                          (make-tree 7 (make-tree 5 '() '()) (make-tree 9 '() (make-tree 11 '() '())))))
(define tree-3 (make-tree 5
                          (make-tree 3 (make-tree 1 '() '()) '())
                          (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '()))))

(lookup 12 tree-1)