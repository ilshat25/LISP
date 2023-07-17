#lang sicp

(define (square x)
  (* x x))

(define (random-in-range low high)
  (let ((diff (- high low)))
    (+ low (random diff))))

(define (monte-carlo trials experiment)
  (define (iter remaining-trials passed-trials)
    (cond ((= remaining-trials 0)
           (/ passed-trials trials))
          ((experiment)
           (iter (- remaining-trials 1) (+ passed-trials 1)))
          (else
           (iter (- remaining-trials 1) passed-trials))))
  (iter trials 0))

(define (estimate-integral p x1 x2 y1 y2 t)
  (let ((S (* (abs (- x1 x2))
              (abs (- y1 y2)))))
    (* S (monte-carlo t (lambda ()
                          (p (random-in-range (min x1 x2)
                                               (max x1 x2))
                              (random-in-range (min y1 y2)
                                               (max y1 y2))))))))

(define (estimate-pi trials)
  (estimate-integral (lambda (x y)
                       (<= (+ (square x) (square y)) 1))
                     -1.0 1.0 -1.0 1.0 trials))

;;; Test
(define (p x y)
  (<= (+ (square (- x 5))
         (square (- y 7)))
      9))
(estimate-integral p 2.0 8.0 4.0 10.0 10000)
(estimate-pi 10000)
