#lang sicp
(#%require sicp-pict)

; Frame definition
;(define (make-frame origin edge1 edge2)
;  (cons origin (cons edge1 edge2)))

;(define (origin-frame frame)
;  (car frame))

;(define (edge1-frame frame)
;  (car (cdr frame)))

;(define (edge2-frame frame)
;  (cdr (cdr frame)))

; Vector definition
;(define (make-vect x y) (cons x y))
;(define (xcor-vect v) (car v))
;(define (ycor-vect v) (cdr v))

;(define (add-vect v u)
;  (make-vect (+ (xcor-vect v) (xcor-vect u))
;             (+ (ycor-vect v) (ycor-vect u))))

;(define (sub-vect v u)
;  (make-vect (- (xcor-vect v) (xcor-vect u))
;             (- (ycor-vect v) (ycor-vect u))))

;(define (scale-vect s v)
;  (make-vect (* s (xcor-vect v))
;             (* s (ycor-vect v))))


;(define (frame-coord-map frame)
;  (lambda (v)
;    (add-vect (origin-frame frame)
;              (add-vect (scale-vect (xcor-vect v)
;                                    (edge1-frame frame))
;                        (scale-vect (ycor-vect v)
;                                    (edge2-frame frame))))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (vector-sub (m corner1) new-origin)
                     (vector-sub (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 1 1)
                     (make-vect 0 0)))
(paint (flip-vert einstein))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1 0.5)
                     (make-vect 0.5 1)))
(paint (shrink-to-upper-right einstein))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))
(paint (rotate90 einstein))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0 0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))
(paint (squash-inwards einstein))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                             (make-vect 0.0 0.0)
                             split-point
                             (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                             split-point
                             (make-vect 1.0 0.0)
                             (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
(paint (beside einstein einstein))
    
