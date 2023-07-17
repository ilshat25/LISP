#lang scheme

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (map-signals p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              null
              sequence))

(define (append-signals seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-signals sequence)
  (accumulate (lambda (x y) (+ 1 y))
              0
              sequence))

; Testing
(define (square x) (* x x))
(define test-seq-1 (list 1 2 3 4 5))
(define test-seq-2 (list 6 7 8 9 0))

(map square test-seq-1)         ; (1 4 9 16 25)
(map-signals square test-seq-1) ; (1 4 9 16 25)

(append test-seq-1 test-seq-2)         ; (1 2 3 4 5 6 7 8 9 0)
(append-signals test-seq-1 test-seq-2) ; (1 2 3 4 5 6 7 8 9 0)

(length test-seq-1)         ; 5
(length-signals test-seq-1) ; 5
