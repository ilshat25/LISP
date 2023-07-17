#lang sicp

(define (cons x y)
  (define (set-x! new-x)
    (set! x new-x))
  (define (set-y! new-y)
    (set! y new-y))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Неопределенная операция -- CONS" m))))
  dispatch)


(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-val) ((z 'set-car!) new-val) z)
(define (set-cdr! z new-val) ((z 'set-cdr!) new-val) z)

(define x (cons 1 2))
(car x)
(cdr x)
(set-car! x 3)
(set-cdr! x 4)
(car x)
(cdr x)
