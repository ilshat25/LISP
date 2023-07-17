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


(define (element-of-set? x set)
  (if (null? set)
      false
      (or (eq? x (car set))
          (element-of-set? x (cdr set)))))

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

(define (my-encode message tree)
  (define (encode-iter message current-branch)
    (cond ((null? message) nil)
          ((not (element-of-set? (car message) (symbols current-branch)))
           (error "плохой символ -- ENCODE" (car message)))
          ((leaf? current-branch)
           (encode-iter (cdr message) tree))
          (else (let ((left (left-branch current-branch))
                      (right (right-branch current-branch)))
                  (if (element-of-set? (car message) (symbols left))
                      (cons 0 (encode-iter message left))
                      (cons 1 (encode-iter message right)))))))
  (encode-iter message tree))

(define (encode message tree)
  (define (encode-symbol symbol tree)
    (if (leaf? tree)
        (if (eq? symbol (symbol-leaf tree))
            nil
            (error "плохой символ -- ENCODE-SYMBOL" symbol))
        (let ((left (left-branch tree))
              (right (right-branch tree)))
          (if (memq symbol (symbols left))
              (cons 0 (encode-symbol symbol left))
              (cons 1 (encode-symbol symbol right))))))
  (if (null? message)
      nil
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; Encode testing
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(encode (decode sample-message sample-tree) sample-tree)
(encode '(A A B D) sample-tree)
(encode '(A E D) sample-tree)


