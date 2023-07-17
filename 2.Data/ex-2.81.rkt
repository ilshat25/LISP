 #lang sicp

; Utility procedures
(define (square x) (* x x))

; Procedures to work with table
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

; Tag related procedures
(define (attach-tag data-tag contents)
  (cons data-tag contents))

(define (data-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Некорректное представление или отсутствие метки -- DATA-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Некорректное представление или отсутствие метки -- CONTENTS" datum)))

;; Rectangular package
(define (install-rectangular-package)
  ; Inner procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z) (sqrt (+ (square (real-part z))
                                 (square (imag-part z)))))
  (define (angel z) (atan (imag-part z)
                          (real-part z)))
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ; Interface to an external program
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angel '(rectangular) angel)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'install-rectangular-package...done)

;; Polar package
(define (install-polar-package)
  ; Inner procedures
  (define (magnitude z) (car z))
  (define (angel z) (cdr z))
  (define (imag-part z)
    (* (magnitude z) (sin (angel z))))
  (define (real-part z)
    (* (magnitude z) (cos (angel z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (make-from-mag-ang r a) (cons r a))

  ; Interface to an external program
  (define (tag x) (attach-tag 'polar x))
  (put 'magnitude '(polar) magnitude)
  (put 'angel '(polar) angel)
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'install-polar-package...done)

; Generic procedures

;; Apply-generic procedure without coercion
;(define (apply-generic op . args)
;  (let ((data-tags (map data-tag args)))
;    (let ((proc (get op data-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (error "Нет подходящей операции под данные аргументы -- APPLY-GENERIC" (list op args))))))

;; Apply-generci procedure with self coercion
(define (apply-generic op . args)
  (define (raise-error)
    (error "Нет подходящей операции под данные аргументы -- APPLY-GENERIC" (list op args)))
  (let ((data-tags (map data-tag args)))
    (let ((proc (get op data-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((t1 (car data-tags))
                    (t2 (cadr data-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (not (eq? t1 t2))
                    (let ((t1->t2 (get-coercion (car data-tags) (cadr data-tags)))
                          (t2->t1 (get-coercion (cadr data-tags) (car data-tags))))
                      (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                            (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                        (else (raise-error))))
                    (raise-error)))
                (raise-error))))))


;;;;;; Generic arithmetical operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (exp x y) (apply-generic 'exp x y))

;;;;;; Arifmetical packages

;;; Default scheme number package
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))

;;; Rational number package from section 2.1.1
(define (install-rational-package)
  ; Initial procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat x y)
    (let ((g (gcd x y)))
      (cons (/ x g) (/ y g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ; External interface
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (x y) (tag (make-rat x y))))
  'done)

(define (make-rational x y)
  ((get 'make 'rational) x y))

;;; Complex number package from 2.4
(define (install-complex-package)
  ; Internal interface
  ; Requires polar and rectangular packages to be installed
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angel z) (apply-generic 'angel z))
  
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add x y)
    (make-from-real-imag (+ (real-part x) (real-part y))
                         (+ (imag-part x) (imag-part y))))
  (define (sub x y)
    (make-from-real-imag (- (real-part x) (real-part y))
                         (- (imag-part x) (imag-part y))))
  (define (mul x y)
    (make-from-mag-ang (* (magnitude x) (magnitude y))
                       (+ (angel x) (angel y))))
  (define (div x y)
    (make-from-mag-ang (/ (magnitude x) (magnitude y))
                       (- (angel x) (angel y))))

  ; External interface
  (define (tag x) (attach-tag 'complex x))
  (put 'add '(complex complex)
       (lambda (x y) (tag (add x y))))
  (put 'sub '(complex complex)
       (lambda (x y) (tag (sub x y))))
  (put 'mul '(complex complex)
       (lambda (x y) (tag (mul x y))))
  (put 'div '(complex complex)
       (lambda (x y) (tag (div x y))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;;; Coercion procedures
(define (scheme-number->complex x)
  (make-complex-from-real-imag (contents x) 0))
(define (scheme-number->scheme-number x) x)
(define (complex->complex z) z)

; (put-coercion 'scheme-number 'complex scheme-number->complex)
; (put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
; (put-coercion 'complex 'complex complex->complex)

; Testing
(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(define x-sch (make-scheme-number 10))
(define y-sch (make-scheme-number 12))
(define x-rat (make-rational 10 12))
(define y-rat (make-rational 7 49))
(define x-com (make-complex-from-real-imag 4 5))
(define y-com (make-complex-from-mag-ang 2 3))

(add x-sch y-sch)
(add x-com y-com)
(add x-sch y-com)
(add x-com y-sch)
(exp x-sch y-sch)
(exp x-com y-com)

