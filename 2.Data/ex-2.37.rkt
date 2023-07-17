#lang scheme

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row)
         (dot-product row v))
       m))

(define (transpose mat)
  (accumulate-n cons null mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col)
                  (dot-product row col))
                cols))
         m)))

; Testing
(define test-vector-1 (list 1 2 3))
(define test-vector-2 (list 4 5 6))
(define test-matrix-1 (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))
(define test-matrix-2 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(dot-product test-vector-1 test-vector-2)     ; 32
(matrix-*-vector test-matrix-2 test-vector-1) ; (14 32 50 68)
(matrix-*-vector test-matrix-2 test-vector-2) ; (32 77 122 167)
(transpose test-matrix-1)                     ; ((1 5 9) (2 6 10) (3 7 11) (4 8 12))
(transpose test-matrix-2)                     ; ((1 4 7 10) (2 5 8 11) (3 6 9 12))
(matrix-*-matrix test-matrix-1 test-matrix-2) ; ((70 80 90) (158 184 210) (246 288 330))
(matrix-*-matrix test-matrix-2 test-matrix-1) ; ((38 44 50 56) (83 98 113 128) (128 152 176 200) (173 206 239 272))
