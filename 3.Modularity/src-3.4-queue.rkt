#lang sicp

(define (front-ptr z) (car z))
(define (rear-ptr z) (cdr z))
(define (set-front-ptr! z new-ptr) (set-car! z new-ptr))
(define (set-rear-ptr! z new-ptr) (set-cdr! z new-ptr))

(define (make-queue) (cons '() '()))
(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "Очередь пуста -- FRONT-QUEUE")
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair))
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair))))
  queue)

(define (delete-queue! queue)
  (if (empty-queue? q)
      (error "Очередь пуста -- DELETE-QUEUE!")
      (set-front-ptr! queue (cdr (front-ptr queue))))
  queue)

(define q (make-queue))
(empty-queue? q)
(insert-queue! q 'a)
(insert-queue! q 'b)
(delete-queue! q)
(insert-queue! q 'c)
(insert-queue! q 'd)
(delete-queue! q)
(delete-queue! q)
(delete-queue! q)
(empty-queue? q)
