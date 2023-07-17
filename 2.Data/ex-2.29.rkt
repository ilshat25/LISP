#lang scheme

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; Test data
(define test-mobile-1 (make-mobile (make-branch 3 6)
                                   (make-branch 2 9)))
(define test-mobile-2 (make-mobile (make-branch 1 4)
                                   (make-branch 3 (make-mobile (make-branch 2 8)
                                                               (make-branch 4 4)))))
(define test-mobile-3 (make-mobile (make-branch 5 test-mobile-1)
                                   (make-branch 5 test-mobile-1)))
(define test-mobile-4 (make-mobile (make-branch 3 test-mobile-1)
                                   (make-branch 2 test-mobile-2)))

; A
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch)))

; B
(define (total-weight mobile)
  (if (pair? mobile)
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))
      mobile))

; Testing
(total-weight test-mobile-1) ; 15
(total-weight test-mobile-2) ; 16
(total-weight test-mobile-3) ; 30
(total-weight test-mobile-4) ; 31

; C
(define (mobile-balanced? mobile)
  (define (branch-torque branch)
    (* (branch-length branch) (total-weight (branch-structure branch))))
  (if (pair? mobile)
      (let ((lb (left-branch mobile))
            (rb (right-branch mobile)))
        (and (= (branch-torque lb)
                (branch-torque rb))
             (mobile-balanced? (branch-structure lb))
             (mobile-balanced? (branch-structure rb))))
      #t))

; Testing
(mobile-balanced? test-mobile-1) ; #t
(mobile-balanced? test-mobile-2) ; #f
(mobile-balanced? test-mobile-3) ; #t
(mobile-balanced? test-mobile-4) ; #f



