#lang sicp

(define (make-account balance password)
  (define ( withdraw amount)
    (if (< balance amount)
        "Недостаточно денег на счете"
        (begin (set! balance (- balance amount))
               balance)))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch try-password m)
    (cond ((not (eq? try-password password)) (lambda (amount) "Неверный пароль")) 
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "неизваестный вызов -- MAKE-ACCOUNT" m))))
  dispatch)

;;; Test
(define acc (make-account 100 'password))

((acc 'password 'withdraw) 20 )
((acc 'no-password 'withdraw) 30)
((acc 'password 'deposit) 30)
((acc 'no-password 'deposit) 100)
