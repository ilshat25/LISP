#lang scheme

(define (odd? num)
  (= (remainder num 2) 1))

(define (even? num)
  (= (remainder num 2) 0))

(define (square num) (* num num))

(define (fib k)
  (define (fib-iter a b n)
    (if (= n k)
        a
        (fib-iter b (+ a b) (+ n 1))))
  (fib-iter 0 1 0))

; Default realization
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        null
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

; Realization using signals
(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares-signals tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs-signals k)
  (accumulate cons
              null
              (filter even?
                      (map fib
                           (enumerate-interval 0 k)))))

; Testing
(define test-tree-1 null)
(define test-tree-2 (list 1 2 3 4 5))
(define test-tree-3 (list 1 2 (list 3 4 5) 6))

(sum-odd-squares test-tree-1)         ; 0
(sum-odd-squares test-tree-2)         ; 35
(sum-odd-squares test-tree-3)         ; 35

(sum-odd-squares-signals test-tree-1) ; 0
(sum-odd-squares-signals test-tree-2) ; 35
(sum-odd-squares-signals test-tree-3) ; 35


(define test-num-1 0)
(define test-num-2 10)
(define test-num-3 20)

(even-fibs test-num-1)         ; (0)
(even-fibs test-num-2)         ; (0 2 8 34)
(even-fibs test-num-3)         ; (0 2 8 34 144 610 2584)

(even-fibs-signals test-num-1) ; (0)
(even-fibs-signals test-num-2) ; (0 2 8 34)
(even-fibs-signals test-num-3) ; (0 2 8 34 144 610 2584)
