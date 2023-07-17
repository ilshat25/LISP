#lang sicp

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        (else
         (error "неизвестный тип выражения -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '*  m2))))

(define (sum? x)
  (cond ((null? x) false)
        ((not (pair? x)) false)
        ((eq? (car x) '+) true)
        (else (sum? (cdr x)))))

(define (one-element-list-check lst)
  (if (null? (cdr lst))
      (car lst)
      lst))

(define (addend s)
  (define (addend-iter rest exp)
    (if (eq? (car exp) '+)
        rest
        (addend-iter (append rest (list (car exp)))
                     (cdr exp))))
  (one-element-list-check (addend-iter '() s)))

(define (augend s)
  (if (eq? (car s) '+)
      (cdr s)
      (one-element-list-check (augend (cdr s)))))

(define (product? x)
  (cond ((null? x) false)
        ((not (pair? x)) false)
        ((eq? (car x) '*) true)
        (else (product? (cdr x)))))

(define (multiplier p)
  (define (multiplier-iter rest exp)
    (if (eq? (car exp) '*)
        rest
        (multiplier-iter (append rest (list (car exp)))
                         (cdr exp))))
  (one-element-list-check (multiplier-iter '() p)))

(define (multiplicand p)
  (if (eq? (car p) '*)
      (cdr p)
      (one-element-list-check (multiplicand (cdr p)))))



; Tests
(deriv '(x + 3) 'x)
(deriv '(x * (x * x)) 'x)
(deriv '(x * y) 'x)
(deriv '((x * y) * (x + 3)) 'x)
(deriv '(x + y + 2) 'x)
(deriv '(x + 3 * (x + y + 2)) 'x)
(deriv '((x * y) * (x + 3)) 'x)