#lang sicp

; Leaf structure
(define  (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))

(define (leaf? object)
  (eq? (car object) 'leaf))


; Tree structure
(define (left-branch tree)
  (if (leaf? tree)
      '()
      (car tree)))

(define (right-branch tree)
  (if (leaf? tree)
      nil
      (cadr tree)))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ((first (car set)))
        (if (> (weight x) (weight first))
            (cons first (adjoin-set x (cdr set)))
            (cons x set)))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; Huffman tree generate
(define (generate-huffman-tree pairs)
  (define (successive-merge set)
    (if (= (length set) 1)
        (car set)
        (successive-merge (adjoin-set (make-code-tree (car set)
                                                      (cadr set))
                                      (cddr set)))))
  (successive-merge (make-leaf-set pairs)))

; Encode testing
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(make-leaf-set '((A 4) (B 2) (D 1) (C 1)))
(generate-huffman-tree '((A 4) (B 2) (D 1) (C 1)))
sample-tree



