 #lang sicp

;;;;; Utility procedures
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

;;;;; Procedures to work with table
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

;;;;; Tag related procedures
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

;;;;; Tower related procedures
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

;;;;; Generic procedures
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
               (if (or (eq? op 'drop) (eq? op 'raise) (eq? op 'equ?) (eq? op '=zero?)) 
                   res
                   (drop res)))) ; try to drop
            ((same? data-tags) (raise-exception))   ; all data-tags are the same
            (else
             (let ((htag (highest-tag (unique data-tags)))) ; get tag with gighest level in tag tower from unique data-tags
               (apply apply-generic (cons op (map (lambda (x) (raise-to htag x)) ; get raised to htag args
                                                  args)))))))))



;;;;; Generic arithmetical operations
(define (add . args) (apply apply-generic (cons 'add args)))
(define (sub . args) (apply apply-generic (cons 'sub args)))
(define (mul . args) (apply apply-generic (cons 'mul args)))
(define (div . args) (apply apply-generic (cons 'div args)))
(define (equ? . args) (apply apply-generic (cons 'equ? args)))
(define (exp x y) (apply-generic 'exp x y))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))
(define (=zero? x) (apply-generic '=zero? x))

(define (make-general target-tag proc)
  (lambda args
    (apply proc (map (lambda (x) (raise-to target-tag x))
                     args))))

(define sine (make-general 'real sin))
(define cosine (make-general 'real cos))
(define arctan (make-general 'real atan))
(define squareroot (make-general 'real sqrt))
(define (square x) (mul x x))

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
  
;;;;; Arifmetical packages

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
  ;; initial procedures
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
  ;; interface for rest of the system
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
  ;; interface for rest of the system
  (put 'add '(real real) +)
  (put 'sub '(real real) -)
  (put 'mul '(real real) *)
  (put 'div '(real real) /)
  (put 'make 'real (lambda (x) (exact->inexact x)))
  (put 'sin '(real) sin)
  (put 'cos '(real) cos)
  (put 'sqrt '(real) sqrt)
  (put 'atan '(real real) atan)
  'real-package-installed)

(define (make-real x)
  ((get 'make 'real) x))

;;; Rectangular package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z) (squareroot (add (square (real-part z))
                                   (square (imag-part z)))))
  (define (angel z) (arctan (imag-part z)
                          (real-part z)))
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))
  ;; interface for rest of the program
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angel '(rectangular) angel)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'rectangular-package-installed)

;;; Polar package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angel z) (cdr z))
  (define (imag-part z)
    (mul (magnitude z) (sine (angel z))))
  (define (real-part z)
    (mul (magnitude z) (cosine (angel z))))
  (define (make-from-real-imag x y)
    (cons (squareroot (add (square x) (square y)))
          (arctan y x)))
  (define (make-from-mag-ang r a) (cons r a))
  ;; interface for rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'magnitude '(polar) magnitude)
  (put 'angel '(polar) angel)
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'polar-package-installed)

;;; Complex number package from 2.4
(define (install-complex-package)
  ;; internal interface
  ;; requires polar and rectangular packages to be installed
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angel z) (apply-generic 'angel z))
  
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex x y)
    (make-from-real-imag (add (real-part x) (real-part y))
                         (add (imag-part x) (imag-part y))))
  (define (sub-complex x y)
    (make-from-real-imag (sub (real-part x) (real-part y))
                         (sub (imag-part x) (imag-part y))))
  (define (mul-complex x y)
    (make-from-mag-ang (mul (magnitude x) (magnitude y))
                       (add (angel x) (angel y))))
  (define (div-complex x y)
    (make-from-mag-ang (div (magnitude x) (magnitude y))
                       (sub (angel x) (angel y))))
  ;; interface for rest of the system 
  (define (tag x) (attach-tag 'complex x))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'add '(complex complex)
       (lambda (x y) (tag (add-complex x y))))
  (put 'sub '(complex complex)
       (lambda (x y) (tag (sub-complex x y))))
  (put 'mul '(complex complex)
       (lambda (x y) (tag (mul-complex x y))))
  (put 'div '(complex complex)
       (lambda (x y) (tag (div-complex x y))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'complex-package-installed)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;;; Polynomial package
(define (install-polynomial-package)
  ;; internal procedures
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? var) (symbol? var))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ; term-list related procedures
  (define (addjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list) (null? term-list))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (addjoin-term t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (addjoin-term t2 (add-terms L1 (rest-terms L2))))
                   (else (addjoin-term (make-term (order t1)
                                                  (add (coeff t1) (coeff t2)))
                                       (add-terms (rest-terms L1)
                                                  (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (cond ((or (empty-termlist? L1) (empty-termlist? L2))
           (the-empty-termlist))
          ((=zero? (coeff (first-term L1)))
           (mul-terms (rest-terms L1) L2))
          (else (add-terms (mul-terms-by-term (first-term L1) L2)
                           (mul-terms (rest-terms L1) L2)))))
  (define (mul-terms-by-term term terms)
    (if (empty-termlist? terms)
        (the-empty-termlist)
        (addjoin-term (make-term (add (order term) (order (first-term terms)))
                                 (mul (coeff term) (coeff (first-term terms))))
                      (mul-terms-by-term term (rest-terms terms)))))
  ; term related procedures
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ; operations
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Многочлены от разных переменных -- ADD-POLY"
               (list p1 p2))))
 
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Многочлены от разных переменных -- MUL-POLY"
               (list p1 p2))))
  ;; interface for rest of the system
  (define (tag x) (attach-tag 'polynomial x))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'polynomial-package-installed)

(define (make-poly variable terms)
  ((get 'make 'polynomial) variable terms))

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

;;; Zero package
(define (install-zero-package)
  ;; rational package procedures
  (define numer (get 'numer '(rational)))
  ;; complex internal package procedures
  (define real-part (get 'real-part '(complex)))
  (define imag-part (get 'imag-part '(complex)))
  ;; interface for rest of the system
  (put '=zero? '(integer) (lambda (x) (= x 0)))
  (put '=zero? '(rational) (lambda (x) (= (numer x) 0)))
  (put '=zero? '(real) (lambda (x) (= x 0)))
  (put '=zero? '(complex) (lambda (x) (and (= (real-part x) 0)
                                           (= (imag-part x) 0))))
  'zero-package-installed)

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
    (real-part x))
  (define (project-real x)     ; to integer
    (make-integer (inexact->exact (round x))))
  (define (project-rational x) ; to integer
    (make-integer (inexact->exact (round (div (numer x) (denom x))))))
  ;; interface to rest of the system
  (put 'project '(complex) project-complex)
  (put 'project '(real) project-real)
  (put 'project '(rational) project-rational)
  'project-package-installed)

; Testing
(install-rectangular-package)
(install-polar-package)
(install-integer-package)
(install-rational-package)
(install-real-package)
(install-complex-package)
(install-equ-package)
(install-zero-package)
(install-raise-package)
(install-project-package)
(install-polynomial-package)

(define p1 (make-poly 'x '((2 0) (1 4))))
(define p2 (make-poly 'x '((1 0) (0 1))))

(add p1 p2)
(mul p1 p2)
