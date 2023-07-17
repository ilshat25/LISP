#lang scheme

(define (abs x) (if (< x 0) (- x) x))

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (enumerate-interval lo hi)
  (if (> lo hi)
      null
      (cons lo (enumerate-interval (+ lo 1) hi))))

(define (safe? k positions)
  (let ((test-pos (car positions))
        (rest-pos (cdr positions)))
    (define (iter positions)
      (if (null? positions)
          #t
          (let ((pos (car positions))
                (rest (cdr positions)))
            (if (or (= (car pos) (car test-pos))
                    (= (cdr pos) (cdr test-pos))
                    (= (- (car pos) (cdr pos))
                       (- (car test-pos) (cdr test-pos)))
                    (= (+ (car pos) (cdr pos))
                       (+ (car test-pos) (cdr test-pos))))
                #f
                (iter rest)))))
    (iter rest-pos)))

(define empty-board null)

(define (adjoin-position row col positions)
  (cons (cons col row) positions))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;Testing
(queens 6)
