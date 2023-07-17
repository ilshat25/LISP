#lang sicp
(#%require sicp-pict)

; 1-st edition
(define (make-frame-1ed origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame-1ed frame)
  (car frame))

(define (edge1-frame-1ed frame)
  (car (cdr frame)))

(define (edge2-frame-1ed frame)
  (car (cdr (cdr frame))))

; Testing
(define frame-1ed (make-frame-1ed (cons 2 2) (cons 3 3) (cons 4 4)))
(origin-frame-1ed frame-1ed)
(edge1-frame-1ed frame-1ed)
(edge2-frame-1ed frame-1ed)

; 2-d edition
(define (make-frame-2ed origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-2ed frame)
  (car frame))

(define (edge1-frame-2ed frame)
  (car (cdr frame)))

(define (edge2-frame-2ed frame)
  (cdr (cdr frame)))

; Testing
(define frame-2ed (make-frame-2ed (cons 2 2) (cons 3 3) (cons 4 4)))
(origin-frame-2ed frame-2ed)
(edge1-frame-2ed frame-2ed)
(edge2-frame-2ed frame-2ed)
