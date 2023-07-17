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

;; Tower related procedures
(define tower '(integer rational real complex))

(define (level-by-tag tag)
  (define (iter stack-tower idx)
    (cond ((null? stack-tower) (error "Данный тип не входит в башню -- LEVEL-BY-TAG" tag))
          ((eq? (car stack-tower) tag) idx)
          (else (iter (cdr stack-tower) (+ idx 1)))))
  (iter tower 1))

(define (tag-by-level lvl)
  (define (iter stack-tower idx)
    (cond ((null? stack-tower) (error "В башне отсутствует данный уровень -- TAG-BY-LEVEL" lvl))
          ((= lvl idx) (car stack-tower))
          (else (iter (cdr stack-tower) (+ idx 1)))))
  (iter tower 1))

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
  (define (highest-tag tags)
    (define (iter tags htag)
      (if (null? tags) htag
          (iter (cdr tags) (if (> (level-by-tag (car tags)) (level-by-tag htag))
                               (car tags)
                               htag))))
    (iter (cdr tags) (car tags)))
  (let ((data-tags (map data-tag args)))
    (let ((proc (get op data-tags)))
      (cond (proc
             (let ((res (apply proc (map contents args)))) ; base case, no coercions needed
               (if (or (eq? op 'drop) (eq? op 'raise) (eq? op 'equ?)) 
                   res
                   (drop res)))) ; try to drop
            ((same? data-tags) (raise-exception))   ; all data-tags are the same
            (else
             (let ((htag (highest-tag (unique data-tags)))) ; get tag with gighest level in tag tower from unique data-tags
               (apply apply-generic (cons op (map (lambda (x) (raise-to htag x)) ; get raised to htag args
                                                  args)))))))))



;;;;;; Generic arithmetical operations
(define (add . args) (apply apply-generic (cons 'add args)))
(define (sub . args) (apply apply-generic (cons 'sub args)))
(define (mul . args) (apply apply-generic (cons 'mul args)))
(define (div . args) (apply apply-generic (cons 'div args)))
(define (equ? . args) (apply apply-generic (cons 'equ? args)))
(define (exp x y) (apply-generic 'exp x y))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))

;; Projection realated procedures
(define (drop x)
  (if (not (get 'project (list (data-tag x))))
      x ; no project procedure presented
      (let ((x-proj (project x)))
        (if (equ? x (raise x-proj)) ; try to rise projection
            (drop x-proj)
            x))))

;; Raise related procedures
(define (raise-by n x)
  (cond ((< n 0) (error "Нельзя поднять на отричательное число -- RAISE-BY" (list n x)))
        ((= n 0) x)
        (else (raise-by (- n 1) (raise x)))))

(define (raise-to target-tag x)
  (let ((target-lvl (level-by-tag target-tag))
        (cur-lvl (level-by-tag (data-tag x))))
    (raise-by (- target-lvl cur-lvl) x)))
  
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
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
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
  ;; rational package procedures
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

;;; Equ package
(define (install-equ-package)
  ;; rational internal package procedures
  (define numer (get 'numer '(rational))) 
  (define denom (get 'denom '(rational)))
  ;; complex internal package procedures
  (define real-part (get 'real-part '(complex)))
  (define imag-part (get 'imag-part '(complex)))
  ;; internal procedures
  (define (equ-rational? x y)
    (equ? (mul (numer x) (denom y))
          (mul (denom x) (numer y))))
  (define (equ-complex? x y)
    (and (equ? (real-part x) (real-part y))
         (equ? (imag-part x) (imag-part y))))
  ;; interface to rest of the system
  (put 'equ? '(integer integer) =)
  (put 'equ? '(rational rational) equ-rational?)
  (put 'equ? '(real real) =)
  (put 'equ? '(complex complex) equ-complex?)
  'equ-package-installed)

;;; Project package
;;; for simplicity I avoided the projection from rational to real
;;; (it's a quite complicate process)
(define (install-project-package)
  ;; rational internal package procedures
  (define numer (get 'numer '(rational))) 
  (define denom (get 'denom '(rational)))
  ;; complex internal package procedures
  (define real-part (get 'real-part '(complex)))
  (define imag-part (get 'imag-part '(complex)))
  ;; internal procedures
  (define (project-complex x)  ; to real
    (make-real (real-part x)))
  (define (project-real x)     ; to integer
    (make-integer (inexact->exact (round x))))
  (define (project-rational x) ; to integer
    (make-integer (inexact->exact (round (div (numer x) (denom x))))))
  ;; interface to rest of the system
  (put 'project '(complex) project-complex)
  (put 'project '(real) project-real)
  (put 'project '(rational) project-rational)
  'project-package-installed)
  

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
(install-equ-package)
(install-raise-package)
(install-project-package)

(define x-sch (make-integer 10))
(define y-sch (make-integer 12))
(define x-rl (make-real 12.2))
(define y-rl (make-real 4.55))
(define x-rat (make-rational 10 12))
(define y-rat (make-rational 7 49))
(define x-com (make-complex-from-real-imag 4 5))
(define y-com (make-complex-from-mag-ang 2 3))

(add x-sch y-sch)
(add x-sch x-rl)
(add x-sch x-rat)
(add x-sch x-com)
(add x-rat x-com)
(add (make-complex-from-real-imag 14 5) (make-complex-from-real-imag 5 -5))

