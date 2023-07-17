#lang sicp

; A. we can't include constants and varaibles because they don't have their own natural, but we can add it outselves

; Utility procedures
(define (=number? exp num)
  (and (number? exp) (= exp num)))

; Procedures to work with table
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; Procedures to work with tags
(define (attach-tag data-tag contents)
  (cons data-tag contents))

(define (data-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Данные не содержат метку типа -- DATA-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Данные не содержат метку типа -- CONTENTS" datum)))


;; Sum package
(define (install-deriv-sum-package)
  ; Inner procedures
  (define (addend exp) (car exp))
  (define (augend exp) (cadr exp))
  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  ; Interface for an external program
  (put 'deriv '+ deriv-sum)
  (put 'make-sum '+ make-sum)
  'done)

;; Product package
(define (install-deriv-product-package)
  ; Inner procedures
  (define (multiplier exp) (car exp))
  (define (multiplicand exp) (cadr exp))
  (define (make-product m1 m2)
    (cond ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((or (=number? m1 0) (=number? m2 0)) 0)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (deriv-product operands var)
    (let ((make-sum (get 'make-sum '+)))
      (make-sum (make-product (multiplier operands)
                              (deriv (multiplicand operands) var))
                (make-product (deriv (multiplier operands) var)
                              (multiplicand operands)))))

  ; Interface for an external program
  (put 'deriv '* deriv-product)
  (put 'make-product '* make-product)
  'done)

;; Exponential package
(define (install-deriv-exponential-package)
  ; Inner procedures
  (define (base exp) (car exp))
  (define (exponent exp) (cadr exp))
  (define (make-exponentiation b e)
    (cond ((=number? b 0) 0)
          ((=number? b 1) 1)
          ((=number? e 0) 0)
          ((=number? e 1) b)
          (else (list '** b e))))
  (define (deriv-exponentiation operands var)
    (let ((make-product (get 'make-product '*))
          (b (base operands))
          (e (exponent operands)))
      (make-product e
                    (make-product (make-exponentiation b
                                                       (- e 1))
                                  (deriv b var)))))

  ;Interface for an external program
  (put 'deriv '** deriv-exponentiation)
  (put 'make-exponentiation '** make-exponentiation)
  'done)

; generic specific functions

(define (apply-generic op . args)
  (let ((data-tags (map data-tag args)))
    (let ((proc (get op data-tags)))
      (if proc
          (apply proc (map contents args))
          (error "Нет подходящего метода для списка аргументов -- APPLY-GENERIC" (list op args))))))

; Deriv realisation
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else
         (let ((proc (get 'deriv (operator exp))))
           (if proc
               (proc (operands exp) var)
               (error "Неизвестный тип выражения -- DERIV" exp))))))

(define (variable? exp) (symbol? exp))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; Testing
(install-deriv-sum-package)
(install-deriv-product-package)
(install-deriv-exponential-package)
(deriv '(** x 8) 'x)

