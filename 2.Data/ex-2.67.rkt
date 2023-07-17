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


; Huffman functions
(define (decode bits tree)
  (define (decode-iter bits current-branch)
    (if (null? bits)
        nil
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-iter (cdr bits) tree))
              (decode-iter (cdr bits) next-branch)))))
  (decode-iter bits tree))

(define (choose-branch bit current-branch)
  (cond ((= bit 0) (left-branch current-branch))
        ((= bit 1) (right-branch current-branch))
        (else (error "плохой бит -- CHOOSE BRANCH" bit))))

; Decode testing
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode '(0) sample-tree)
(decode '(1 0) sample-tree)
(decode '(1 1 0 0 0) sample-tree)
(decode '(1 1 1) sample-tree)
(decode sample-message sample-tree)

