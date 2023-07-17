#lang scheme

(define (scale-list-1 items factor)
  (if (null? items)
      null
      (cons (* (car items) factor)
            (scale-list-1 (cdr items) factor))))

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list-2 items factor)
  (map (lambda (x) (* x factor))
       items))

; Testing

(map abs (list -10 2.5 -11.6 17))
; (10 2.5 11.6 17)
(map (lambda (x) (* x x))
     (list 1 2 3 4))
; (1 4 9 16)

(scale-list-1 (list 1 2 3 4 5) 10)
; (10 20 30 40 50)
(scale-list-2 (list 1 2 3 4 5) 10)
; (10 20 30 40 50)
