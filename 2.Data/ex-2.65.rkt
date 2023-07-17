#lang sicp

(define (intersection-list-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-list-set (cdr set1)
                                               (cdr set2))))
              ((< x1 x2)
               (intersection-list-set (cdr set1) set2))
              ((> x1 x2)
               (intersection-list-set set1 (cdr set2)))))))

(define (union-list-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-list-set (cdr set1)
                                                (cdr set2))))
                      ((< x1 x2)
                       (cons x1 (union-list-set (cdr set1) set2)))
                      ((> x1 x2)
                       (cons x2 (union-list-set set1 (cdr set2)))))))))

; Tree
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (make-tree-using-list-function set1 set2 list-func)
  (let ((list-set1 (tree->list set1))
        (list-set2 (tree->list set2)))
    (let ((list-result (list-func list-set1
                                  list-set2)))
      (list->tree list-result))))

(define (intersection-tree-set set1 set2)
  (make-tree-using-list-function set1 set2 intersection-list-set))

(define (union-tree-set set1 set2)
  (make-tree-using-list-function set1 set2 union-list-set))


; Convert functions
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree list)
  (car (partial-tree list (length list))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient n 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (non-entry (cdr right-result)))
                (cons (make-tree entry left-tree right-tree)
                      non-entry))))))))

; Testing
(define tree-1 (make-tree 7
                          (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
                          (make-tree 9 '() (make-tree 11 '() '()))))
(define tree-2 (make-tree 3
                          (make-tree 1 '() '())
                          (make-tree 8 (make-tree 5 '() '()) (make-tree 9 '() (make-tree 14 '() '())))))
(define tree-3 (make-tree 5
                          (make-tree 3 (make-tree 1 '() '()) '())
                          (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '()))))

(intersection-tree-set tree-1 tree-2)
(union-tree-set tree-1 tree-2)

