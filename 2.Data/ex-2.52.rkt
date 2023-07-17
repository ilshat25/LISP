#lang sicp
(#%require sicp-pict)

; A
(paint (segments->painter
        (list
         (make-segment (make-vect 0.20 0.00) (make-vect 0.35 0.50))
         (make-segment (make-vect 0.35 0.50) (make-vect 0.30 0.60))
         (make-segment (make-vect 0.30 0.60) (make-vect 0.15 0.45))
         (make-segment (make-vect 0.15 0.45) (make-vect 0.00 0.60))
         (make-segment (make-vect 0.00 0.80) (make-vect 0.15 0.65))
         (make-segment (make-vect 0.15 0.65) (make-vect 0.30 0.70))
         (make-segment (make-vect 0.30 0.70) (make-vect 0.40 0.70))
         (make-segment (make-vect 0.40 0.70) (make-vect 0.35 0.85))
         (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00))
         (make-segment (make-vect 0.60 1.00) (make-vect 0.65 0.85))
         (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.70))
         (make-segment (make-vect 0.60 0.70) (make-vect 0.75 0.70))
         (make-segment (make-vect 0.75 0.70) (make-vect 1.00 0.40))
         (make-segment (make-vect 1.00 0.20) (make-vect 0.60 0.48))
         (make-segment (make-vect 0.60 0.48) (make-vect 0.80 0.00))
         (make-segment (make-vect 0.40 0.00) (make-vect 0.50 0.30))
         (make-segment (make-vect 0.50 0.30) (make-vect 0.60 0.00))
         (make-segment (make-vect 0.42 0.85) (make-vect 0.50 0.80))
         (make-segment (make-vect 0.50 0.80) (make-vect 0.58 0.85)))))

; B
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((lower (up-split painter (- n 1))))
      (below painter (beside lower lower)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((lower (right-split painter (- n 1))))
        (beside painter (below lower lower)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (beside (below painter
                     (up-split painter (- n 1)))
              (below (right-split painter (- n 1))
                     (corner-split painter (- n 1))))))

(paint (corner-split einstein 2))

; C
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((corner (corner-split painter n)))
    ((square-of-four flip-vert
                     rotate180
                     identity
                     flip-horiz) corner)))

(paint (square-limit einstein 2))