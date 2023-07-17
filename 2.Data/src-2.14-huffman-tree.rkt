#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (symbol-leaf tree)
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; Set operations

(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ((first-element (car set)))
        (if (> (weight x) (weight first-element))
            (cons first-element
                  (adjoin-set x (cdr set)))
            (cons x set)))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((first (car pairs)))
        (adjoin-set (make-leaf (car first)
                               (cadr first))
                    (make-leaf-set (cdr pairs))))))
      

; Decode Huffman code using Huffman tree
(define (decode bits tree)
  (define (decode-iter bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-iter (cdr bits) tree))
              (decode-iter (cdr bits) next-branch)))))
  (decode-iter bits tree))

(define (choose-branch bit tree)
  (cond ((= bit 0) (left-branch tree))
        ((= bit 1) (right-branch tree))
        (else (error "плохой бит -- CHOOSE-BRANCH" bit))))

; Testing
(make-leaf-set '((A 2) (B 3) (C 5) (D 10)))



         