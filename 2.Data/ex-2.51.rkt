#lang sicp
(#%require sicp-pict)

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                            (vector-sub (m corner1) new-origin)
                            (vector-sub (m corner2) new-origin)))))))

(define (below-1ed painter1 painter2)
  (let ((paint-bottom (transform-painter painter1
                                         (make-vect 0 0)
                                         (make-vect 1 0)
                                         (make-vect 0 0.5)))
        (paint-top (transform-painter painter2
                                      (make-vect 0 0.5)
                                      (make-vect 1 0.5)
                                      (make-vect 0 1))))
    (lambda (frame)
      (paint-bottom frame)
      (paint-top frame))))

(define (below-2ed painter1 painter2)
  (rotate270 (beside (rotate90 painter1) (rotate90 painter2))))

(paint (below-1ed einstein einstein))
(paint (below-2ed einstein einstein))