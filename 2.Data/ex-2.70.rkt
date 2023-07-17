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

; encode and decode Huffman functions
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
          ((not (memq (car message) (symbols current-branch)))
           (error "плохой символ -- ENCODE" (car message)))
          ((leaf? current-branch)
           (encode-iter (cdr message) tree))
          (else (let ((left (left-branch current-branch))
                      (right (right-branch current-branch)))
                  (if (memq (car message) (symbols left))
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



; Program
(define alphabet '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
(define song-tree (generate-huffman-tree alphabet))

(define song-lines '(GET A JOB
                     SHA NA NA NA NA NA NA NA NA
                     GET A JOB
                     SHA NA NA NA NA NA NA NA NA
                     WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                     SHA BOOM))
(write "Huffman 'encoding length:")
(write (length (encode song-lines song-tree)))
(write "Minimum fixed-length encoding length:")
(write (* (length song-lines) 3))







