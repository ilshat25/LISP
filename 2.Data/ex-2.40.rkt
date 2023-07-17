#lang scheme

(define (square x) (* x x))
(define (divides? a b) (= (remainder a b) 0))

(define (prime? n)
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? n test-divisor) test-divisor)
          (else (find-divisor (+ test-divisor 1)))))
  (= n (find-divisor 2)))

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (enumerate-interval lo hi)
  (if (> lo hi)
      null
      (cons lo (enumerate-interval (+ lo 1) hi))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))



; Testing
(unique-pairs 5)
(prime-sum-pairs 10)