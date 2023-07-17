#lang scheme

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval lo hi)
  (if (> lo hi)
      null
      (cons lo (enumerate-interval (+ lo 1) hi))))

(define (flatmap procedure sequence)
  (accumulate append null (map procedure sequence)))

(define (sum seq)
  (accumulate + 0 seq))

(define (equal-sum? seq s)
  (= (sum seq) s))

(define (unique-triplets n)
  (flatmap (lambda (i)
         (flatmap (lambda (j)
                (map (lambda (k)
                       (list i j k))
                     (enumerate-interval 1 (- j 1))))
                (enumerate-interval 1 (- i 1))))
              (enumerate-interval 1 n)))

(define (triplets-of-sum n s)
  (define (equal-s? triplet)
    (= (sum triplet) s))
  (filter equal-s? (unique-triplets n)))

; Testing
(unique-triplets 5)
(triplets-of-sum 10 12)
