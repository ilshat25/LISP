 #lang sicp

; Utility procedures
(define (square x) (* x x))

(define (fold-left init f seq)
  (if (null? seq)
      init
      (fold-left (f init (car seq))
                 f
                 (cdr seq))))

(define (fold-right init f seq)
  (if (null? seq)
      init
      (f (car seq)
         (fold-right init f (cdr seq)))))

(define (filter pred seq)
  (fold-right '()
              (lambda (x res) (if (pred x) (cons x res) res))
              seq))


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
  (cond ((number? datum) (if (exact? datum) 'integer 'real))
        ((pair? datum) (car datum))
        (else (error "Некорректное представление или отсутствие метки -- DATA-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Некорректное представление или отсутствие метки -- CONTENTS" datum))))

(define (tag-eq? t1 t2)
  (eq? t1 t2))

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
(define (apply-generic op . args)
  (define (raise-exception)
    (error "Нет подходящей операции - APPLY-GENERIC" (list op args)))
  (define (same? data)
    (if (null? data)
        true
        (fold-left true
                   (lambda (res x) (and res (eq? x (car data))))
                   data)))
  (define (unique data)
    (define (contains? datum seq)
      (cond ((null? seq) false)
            ((eq? datum (car seq)) true)
            (else (contains? datum (cdr seq)))))
    (fold-left '()
               (lambda (res datum)
                 (if (contains? datum res)
                     res
                     (append res (list datum))))
               data))
  (define (try-coerce-to target tag)
    (if (eq? target tag)
        true
        (let ((coercion (get-coercion tag target)))
          (if coercion true false))))
  (define (try-op-to target)
    (let ((proc (get op (map (lambda (x) target)
                             args))))
      (if proc true false)))

  (let ((data-tags (map data-tag args)))
    (let ((proc (get op data-tags)))
      (cond (proc (apply proc (map contents args))) ; base case, no coercions needed
            ((same? data-tags) (raise-exception))   ; all data-tags are the same
            (else
             (let ((uniq-data-tags (unique data-tags))) ; get unique data-tags
               (let ((can-be-coerced (filter (lambda (target) ; get unique data-tags to that all others can be coerced
                                               (fold-left true
                                                          (lambda (res tag) (and res (try-coerce-to target tag)))
                                                          uniq-data-tags))
                                             uniq-data-tags)))
                 (let ((can-be-operated (filter (lambda (tag) (try-op-to tag)) ; get data tags that can be used with 'op operation
                                                can-be-coerced)))
                   (if (null? can-be-operated)
                       (raise-exception) ; if no tags - error
                       (let ((coerced-args (map (lambda (x) ; get coerced args
                                                  (if (eq? (data-tag x) (car can-be-operated))
                                                      x
                                                      ((get-coercion (data-tag x) (car can-be-operated)) x)))
                                                args)))
                         (apply (get op (map data-tag coerced-args))
                                (map contents coerced-args))))))))))))



;;;;;; Generic arithmetical operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (exp x y) (apply-generic 'exp x y))
(define (raise x) (apply-generic 'raise x))

;;;;;; Arifmetical packages

;;; Integer number package
(define (install-integer-package)
  (put 'add '(integer integer) +)
  (put 'sub '(integer integer) -)
  (put 'mul '(integer integer) *)
  (put 'div '(integer integer) /)
  (put 'exp '(integer integer) expt)
  (put 'make 'integer (lambda (x) x))
  'integer-packae-installed)

(define (make-integer x)
  ((get 'make 'integer) x))

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
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
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
  'rational-package-installed)

(define (make-rational x y)
  ((get 'make 'rational) x y))

;;; Real number package
(define (install-real-package)
  (put 'add '(real real) +)
  (put 'sub '(real real) -)
  (put 'mul '(real real) *)
  (put 'div '(real real) /)
  (put 'make 'real (lambda (x) (exact->inexact x)))
  'real-package-installed)

(define (make-real x)
  ((get 'make 'real) x))

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
  'complex-package-installed)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;;; Raise package
(define (install-raise-package)
  ;; impoer from rational numbers
  (define numer (get 'numer '(rational)))
  (define denom (get 'denom '(rational)))
  ;; interanl procedures
  (define (raise-integer x) (make-rational (contents x) 1))
  (define (raise-rational x) (make-real (div (numer x) (denom x))))
  (define (raise-real x) (make-complex-from-real-imag (contents x) 0))
  ;; interface to rest of the system
  (put 'raise '(integer) raise-integer)
  (put 'raise '(rational) raise-rational)
  (put 'raise '(real) raise-real)
  'raise-package-installed)


;;; Coercion procedures
(define (integer->complex x)
  (make-complex-from-real-imag (contents x) 0))
(define (integer->rational x)
  (make-rational (contents x) 1))
(define (integer->real x)
  (make-real (contents x)))

(put-coercion 'integer 'complex integer->complex)
(put-coercion 'integer 'rational integer->rational)
(put-coercion 'integer 'real integer->real)

; Testing
(install-rectangular-package)
(install-polar-package)
(install-integer-package)
(install-rational-package)
(install-real-package)
(install-complex-package)
(install-raise-package)

(define x-sch (make-integer 10))
(define y-sch (make-integer 12))
(define x-rl (make-real 12.2))
(define y-rl (make-real 4.55))
(define x-rat (make-rational 10 12))
(define y-rat (make-rational 7 49))
(define x-com (make-complex-from-real-imag 4 5))
(define y-com (make-complex-from-mag-ang 2 3))

(raise x-sch)
(raise x-rat)
(raise x-rl)

(raise (raise (raise x-sch)))

