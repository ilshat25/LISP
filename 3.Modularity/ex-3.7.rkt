#lang sicp

;;; Account object
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        (error "Недостаточно средств на балансе --WITHDRAW" amount)))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (display) balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'display) display)
          (else (error "Операция отсутствует --DISPATCH" m))))
  dispatch)

;;; Password account wrapper
(define (make-password-account acc password)
  (define (dispatch try-password m)
    (if (eq? try-password password)
        (cond ((eq? m 'get-account) acc)
              (else (acc m)))
        (error "Неверный пароль --DISPATCH" m)))
  dispatch)

;;; Create joint password account 
(define (make-joint pass-acc old-password new-password)
  (let ((acc (pass-acc old-password 'get-account)))
    (make-password-account acc new-password)))

;;; Test
(define W1 (make-password-account (make-account 100) 'W1P))
(define W2 (make-password-account (make-account 100) 'W2P))
(define W3 (make-joint W1 'W1P 'W3P))

((W1 'W1P 'withdraw) 50)
((W1 'W1P 'display))
((W2 'W2P 'display))
((W3 'W3P 'display))
(newline)
((W2 'W2P 'withdraw) 20)
((W1 'W1P 'display))
((W2 'W2P 'display))
((W3 'W3P 'display))
(newline)
((W1 'W1P 'deposit) 20)
((W1 'W1P 'display))
((W2 'W2P 'display))
((W3 'W3P 'display))
(newline)
((W2 'W2P 'deposit) 50)
((W1 'W1P 'display))
((W2 'W2P 'display))
((W3 'W3P 'display))
        