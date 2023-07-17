#lang scheme

;(define (deep-reverse items)
;  (define (deep-reverse-iter items result)
;    (cond ((null? items) result)
;          ((pair? (car items))
;           (deep-reverse-iter (cdr items)
;                              (cons (deep-reverse (car items))
;                                    result)))
;          (else
;           (deep-reverse-iter (cdr items)
;                              (cons (car items)
;                                    result)))))
;  (deep-reverse-iter items null))

(define (deep-reverse items)
  (define (deep-reverse-iter items result)
    (if (null? items)
        result
        (let ((first (car items))
              (others (cdr items)))
          (deep-reverse-iter others
                             (cons (if (pair? first)
                                       (deep-reverse first)
                                       first)
                                   result)))))
  (deep-reverse-iter items null))

; Testing

(define test-list-1 null)
(define test-list-2 (list 1))
(define test-list-3 (list 1 2 3 4 5 6))
(define test-list-4 (list 1 (list 2 3 4) 5))
(define test-list-5 (list (list 1 2) (list (list 3 4) (list 5 6)) (list 7 8)))

(deep-reverse test-list-1) ; ()
(deep-reverse test-list-2) ; (1)
(deep-reverse test-list-3) ; (6 5 4 3 2 1)
(deep-reverse test-list-4) ; (5 (4 3 2) 1)
(deep-reverse test-list-5) ; ((8 7) ((6 5) (4 3)) (2 1))
