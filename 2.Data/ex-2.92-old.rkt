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
(define tags '(interger rational real rectangular polar complex term sparse dense polynomial))
(define (attach-tag data-tag contents)
  (cons data-tag contents))

(define (data-tag datum)
  (cond ((number? datum) (if (exact? datum) 'integer 'real))
        ((pair? datum) (car datum))
        (else (error "Некорректное представление или отсутствие метки -- DATA-TAG" datum))))

(define (has-tag? datum)
  (or (number? datum) (pair? datum)))

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

(define (in-hierarchy? type)
  (fold-left true
             (lambda (res x) (and res (eq? x type)))
             tower))

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
               (if (or (not (has-tag? res)) (not (in-hierarchy? (data-tag res))) (eq? op 'drop) (eq? op 'raise)) 
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
(define (neg x) (apply-generic 'neg x))
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
  (put 'equ? '(integer integer) =)
  (put '=zero? '(integer)
       (lambda (x) (= x 0)))
  (put 'neg '(integer) -)
  (put 'exp '(integer integer) expt)
  (put 'make 'integer
       (lambda (x) x))
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
  (define (equ-rational? x y)
    (equ? (mul (numer x) (denom y))
          (mul (denom x) (numer y))))
  (define (neg x) (make-rat (- (numer x)) (denom x)))
  ;; interface for rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'equ? '(rational rational) equ-rational?)
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put 'neg '(rational)
       (lambda (x) (tag (neg x))))
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
  (put '=zero? '(real)
       (lambda (x) (= x 0)))
  (put 'equ? '(real real) =)
  (put 'neg '(real) -)
  (put 'make 'real
       (lambda (x) (exact->inexact x)))
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
  ;; dependencies
  (install-rectangular-package)
  (install-polar-package)
  ;; import from dependency packages
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angel z) (apply-generic 'angel z))
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  ;; complex arithmetic operations
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
  (define (equ-complex? x y)
    (and (equ? (real-part x) (real-part y))
         (equ? (imag-part x) (imag-part y))))
  (define (neg-complex x)
    (make-from-real-imag (neg (real-part x))
                         (neg (imag-part x))))
  ;; interface for rest of the system 
  (define (tag x) (attach-tag 'complex x))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put '=zero? '(complex)
       (lambda (x) (and (= (real-part x) 0)
                        (= (imag-part x) 0))))
  (put 'equ? '(complex complex) equ-complex?)
  (put 'neg '(complex)
       (lambda (x) (tag (neg-complex x))))
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

;;;;; Common term package 
(define (install-term-package)
  ;; internal procedures
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (equ-term? t1 t2)
    (and (equ? (order t1) (order t2))
         (equ? (coeff t1) (coeff t2))))
  (define (neg-term term)
    (make-term (order term)
               (neg (coeff term)))) 
  ;; interface for rest of the program
  (define (tag x) (attach-tag 'term x))
  (put 'order '(term) order)
  (put 'coeff '(term) coeff)
  (put 'equ-term? '(term term) equ-term?)
  (put 'neg '(term)
       (lambda (x) (tag (neg-term x))))
  (put 'make 'term
       (lambda (order coeff) (tag (make-term order coeff))))
  'term-package-installed)


;;;;; Sparse termlist package
(define (install-sparse-termlist-package)
  ;; !requires term package to be installed
  ;; dependencies
  (define make-term (get 'make 'term))
  (define (order x) (apply-generic 'order x))
  (define (coeff x) (apply-generic 'coeff x))
  ;; internal procedures
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (the-empty-termlist) '())
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  ;; interface for rest of the system
  (define (tag x) (attach-tag 'sparse x))
  (put 'first-term '(sparse) first-term)
  (put 'rest-terms '(sparse)
       (lambda (x) (tag (rest-terms x))))
  (put 'empty-termlist? '(sparse) empty-termlist?)
  (put 'the-empty-termlist 'sparse
       (lambda () (tag (the-empty-termlist))))
  (put 'adjoin-term '(term sparse)
       (lambda (t l) (tag (adjoin-term (attach-tag 'term t) l))))
  'sparse-termlist-package-installed)

;;;;; Dense termlist package
(define (install-dense-termlist-package)
  ;; !requires term package to be installed
  ;; dependencies
  (define make-term (get 'make 'term))
  (define (order x) (apply-generic 'order x))
  (define (coeff x) (apply-generic 'coeff x))
  ;; internal procedures
  (define (first-term term-list)
    (make-term (- (length term-list) 1)
               (car term-list)))
  (define (rest-terms term-list)
    (if (empty-termlist? term-list)
        (the-empty-termlist)
        (let ((rest (cdr term-list)))
          (cond ((empty-termlist? rest) (the-empty-termlist))
                ((=zero? (coeff (first-term rest))) (rest-terms rest))
                (else rest)))))
  (define (empty-termlist? term-list) (null? term-list))
  (define (the-empty-termlist) '())
  (define (adjoin-term term term-list)
    (cond ((=zero? (coeff term)) term-list)
          ((= (length term-list) (order term))
           (cons (coeff term) term-list))
          (else (adjoin-term term (cons 0 term-list)))))
  ;; interface for rest of the system
  (define (tag x) (attach-tag 'dense x))
  (put 'first-term '(dense) first-term)
  (put 'rest-terms '(dense)
       (lambda (x) (tag (rest-terms x))))
  (put 'empty-termlist? '(dense) empty-termlist?)
  (put 'the-empty-termlist 'dense
       (lambda () (tag (the-empty-termlist))))
  (put 'adjoin-term '(term dense)
       (lambda (x y) (tag (adjoin-term (attach-tag 'term x) y))))
  'dense-termlist-packaje-installed)

;;;;; Polynomial package
(define (install-polynomial-package)
  ;; dependencies
  (install-term-package)
  (install-sparse-termlist-package)
  (install-dense-termlist-package)
  ;; import from dependency packages
  (define make-term (get 'make 'term))
  (define (order x) (apply-generic 'order x))
  (define (coeff x) (apply-generic 'coeff x))
  (define (eq-term? x y) (apply-generic 'equ? x y))
  (define (neg-term x) (apply-generic 'neg x))
  (define (first-term x) (apply-generic 'first-term x))
  (define (rest-terms x) (apply-generic 'rest-terms x))
  (define (empty-termlist? x) (apply-generic 'empty-termlist? x))
  (define the-empty-termlist (get 'the-empty-termlist 'sparse)) ; sparse empty termlist by default
  (define (adjoin-term x y) (apply-generic 'adjoin-term x y))
  ;; internal procedures
  ;; term arithmetic operations on termlists
  (define (eq-termlist? L1 L2)
    (cond ((and (empty-termlist? L1) (empty-termlist? L2)) true)
          ((or (empty-termlist? L1) (empty-termlist? L2)) false)
          ((eq-term? (first-term L1) (first-term L2))
           (eq-termlist? (rest-terms L1) (rest-terms L2)))
          (else false)))
  (define (neg-termlist term-list)
    (if (empty-termlist? term-list)
        (the-empty-termlist)
        (adjoin-term (neg-term (first-term term-list))
                     (neg-termlist (rest-terms term-list)))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                   (else (adjoin-term (make-term (order t1)
                                                 (add (coeff t1) (coeff t2)))
                                      (add-terms (rest-terms L1)
                                                 (rest-terms L2)))))))))
  (define (sub-terms L1 L2)
    (add-terms L1 (neg-termlist L2)))
  (define (mul-terms L1 L2)
    (cond ((or (empty-termlist? L1) (empty-termlist? L2))
           (the-empty-termlist))
          ((=zero? (coeff (first-term L1)))
           (mul-terms (rest-terms L1) L2))
          (else (add-terms (mul-termlist-by-term (first-term L1) L2)
                           (mul-terms (rest-terms L1) L2)))))
  (define (div-terms L1 L2)
    (cond ((empty-termlist? L1)
           (cons (the-empty-termlist) (the-empty-termlist)))
          ((empty-termlist? L2)
           (error "Деление на ноль -- DIV-TERMS" (list L1 L2)))
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (if (< (order t1) (order t2))
                 (cons (the-empty-termlist) L1)
                 (let ((c (make-term (- (order t1) (order t2))
                                     (div (coeff t1) (coeff t2)))))
                   (let ((rest-of-result (div-terms (sub-terms L1 (mul-termlist-by-term c L2))
                                                    L2)))
                     (cons (adjoin-term c (car rest-of-result))
                           (cdr rest-of-result)))))))))
  (define (mul-termlist-by-term term terms)
    (if (empty-termlist? terms)
        (the-empty-termlist)
        (adjoin-term (make-term (add (order term) (order (first-term terms)))
                                (mul (coeff term) (coeff (first-term terms))))
                     (mul-termlist-by-term term (rest-terms terms)))))
  ;; variable specific procedures
  (define (variable? var) (symbol? var))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (neg-poly p) (make-poly (variable p)
                                  (neg-termlist (term-list p))))
  
  (define (eq-poly? p1 p2)
    (and (same-variable? (variable p1) (variable p2))
         (eq-termlist? (term-list p1) (term-list p2))))
  ; poly arithmetic operations
  (define (operation-poly op-name op)
    (lambda (p1 p2)
      (if (same-variable? (variable p1) (variable p2))
          (make-poly (variable p1)
                     (op (term-list p1)
                         (term-list p2)))
          (error "Многочлены от разных переменных --"
                 op-name
                 (list p1 p2)))))
  (define (add-poly p1 p2) ((operation-poly 'ADD-POLY add-terms) p1 p2))
  (define (mul-poly p1 p2) ((operation-poly 'MUL-POLY mul-terms) p1 p2))
  (define (sub-poly p1 p2) ((operation-poly 'SUB-POLY sub-terms) p1 p2))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((term-res (div-terms (term-list p1)
                                   (term-list p2))))
          (list (make-poly (variable p1)
                           (car term-res))
                (make-poly (variable p1)
                           (cdr term-res))))
        (error "Многочлены от разных переменных -- DIV-POLY" (list p1 p2)))) 
  ;; interface for rest of the system
  (define (tag x) (attach-tag 'polynomial x))
  (put 'variable '(polynomial) variable)
  (put 'term-list '(polynomial) term-list)
  (put '=zero? '(polynomial)
       (lambda (x) (empty-termlist? (term-list x))))
  (put 'neg '(polynomial)
       (lambda (p) (tag (neg-poly p))))
  (put 'equ? '(polynomial polynomial) eq-poly?)
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (map tag (div-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'polynomial-package-installed)

(define (make-poly variable terms)
  ((get 'make 'polynomial) variable terms))

;;; Multivariate polynomial package
(define (install-multivariate-polynomial-package)
  ;; dependencies
  (install-polynomial-package)
  ;; import from dependency packages
  (define make-term (get 'make 'term))
  (define (coeff x) (apply-generic 'coeff x))
  (define (order x) (apply-generic 'order x))
  (define (term-var x) (apply-generic 'coeff x))
  (define (first-term x) (apply-generic 'first-term x))
  (define (rest-terms x) (apply-generic 'rest-terms x))
  (define (empty-termlist? x) (apply-generic 'empty-termlist? x))
  (define the-empty-termlist (get 'the-empty-termlist 'sparse))
  (define (adjoin-term x y) (apply-generic 'adjoin-term x y))
  (define (variable x) (apply-generic 'variable x))
  (define (term-list x) (apply-generic 'term-list x))
  ;; internal procedures
  (install-polynomial-package)
  ;; multiplier related procedures
  (define (contains-variable? multiplier)
    (pair? multiplier))
  (define (make-multiplier var pow)
    (cons var pow))
  (define (multiplier-variable multiplier)
    (if (contains-variable? multiplier)
        (car multiplier)
        multiplier))
  (define (multiplier-power multiplier)
    (if (contains-variable? multiplier)
        (cdr multiplier)
        multiplier))
  ;; compare procedures
  (define valid-variables '(x y z t))
  (define (variable=? v1 v2)
    (eq? v1 v2))
  (define (variable<? v1 v2)
    (define (iter vars)
      (cond ((null? vars) (error "Недопустимое название переменной -- VARIABLE<?" (list v1 v2)))
            ((eq? v2 (car vars)) false)
            ((eq? v1 (car vars)) true)
            (else (iter (cdr vars)))))
    (iter valid-variables))
  (define (multiplier=? m1 m2)
    (cond ((and (not (contains-variable? m1)) (not (contains-variable? m2)))
           (equ? m1 m2))
          ((or (not (contains-variable? m1)) (not (contains-variable? m2)))
           false)
          (else (and (variable=? (multiplier-variable m1) (multiplier-variable m2))
                     (= (multiplier-power m1) (multiplier-power m2))))))
  (define (multiplier>? m1 m2)
    (cond ((and (not (contains-variable? m1)) (not (contains-variable? m2)))
           (> m1 m2))
          ((not (contains-variable? m1)) false)
          ((not (contains-variable? m2)) true)
          ((variable=? (multiplier-variable m1) (multiplier-variable m2))
           (> (multiplier-power m1) (multiplier-power m2)))
          (else (variable<? (multiplier-variable m1) (multiplier-variable m2)))))
  ;; monomial related procedures
  (define (make-monomial multiplier) multiplier)
  (define (first-multiplier monomial)
    (if (contains-variable? monomial)
        (car monomial)
        monomial))
  (define (rest-multipliers monomial)
    (cdr monomial))
  (define (factor monomial)
    (if (contains-variable? monomial)
        (factor (rest-multipliers monomial))
        monomial))
  (define (add-factor monomial delta)
    (if (contains-variable? monomial)
        (cons (first-multiplier monomial)
              (add-factor (rest-multipliers monomial)
                          delta))
        (+ monomial delta)))
  (define (adjoin-multiplier multiplier monomial)
    (cond ((= (multiplier-power multiplier) 0) monomial)
          ((not (contains-variable? monomial))
           (if (not (contains-variable? multiplier))
               (mul multiplier monomial)
               (cons multiplier monomial)))
          (else (let ((first (first-multiplier monomial))
                      (rest (rest-multipliers monomial)))
                  (cond ((variable=? (multiplier-variable multiplier) (multiplier-variable first))
                         (cons (make-multiplier (multiplier-variable multiplier)
                                                (+ (multiplier-power multiplier) (multiplier-power first)))
                               rest))
                        ((variable<? (multiplier-variable multiplier) (multiplier-variable first))
                         (cons multiplier monomial))
                        (else (cons first (adjoin-multiplier multiplier rest))))))))
  ;; monomial-list related procedures
  (define (first-monomial monomial-list)
    (car monomial-list))
  (define (rest-monomials monomial-list)
    (cdr monomial-list))
  (define (empty-monomiallist? monomial-list)
    (null? monomial-list))
  ;; compare procedures
  (define (monomial=? m1 m2) ; monomials ((x 8) 9) and ((x 8) 11) are equal, consider only variable part
    (cond ((and (not (contains-variable? m1)) (not (contains-variable? m2)))
           true)
          ((or (not (contains-variable? m1)) (not (contains-variable? m2)))
           false)
          (else (and (multiplier=? (first-multiplier m1)
                                   (first-multiplier m2))
                     (monomial=? (rest-multipliers m1)
                                 (rest-multipliers m2))))))
  (define (monomial>? m1 m2)
    (cond ((and (not (contains-variable? m1)) (not (contains-variable? m2))) false) ; equal
          ((not (contains-variable? m1)) false)
          ((not (contains-variable? m2)) true)
          ((multiplier=? (first-multiplier m1) (first-multiplier m2))
           (monomial>? (rest-monomials m1) (rest-monomials m2)))
          (else (multiplier>? (first-multiplier m1) (first-multiplier m2)))))
  (define (adjoin-monomial monomial monomial-list)
    (if (empty-monomiallist? monomial-list)
        (list monomial)
        (let ((first (first-monomial monomial-list))
              (rest (rest-monomials monomial-list)))
          (cond ((monomial=? monomial first)
                 (cons (add-factor first (factor monomial)) rest))
                ((monomial>? monomial first)
                 (cons monomial monomial-list))
                (else (cons first (adjoin-monomial monomial rest)))))))
  (define (append-monomiallist ML1 ML2)
    (if (null? ML1)
        ML2
        (append-monomiallist (rest-monomials ML1)
                             (adjoin-monomial (first-monomial ML1)
                                              ML2))))
  (define (expansion p)
    (define (iter term-list)
      (if (empty-termlist? term-list)
          nil
          (let ((t (first-term term-list)))
            (let ((m (make-multiplier (variable p) (order t))))
              (if (eq? (data-tag (coeff t)) 'polynomial)
                  (append-monomiallist (map (lambda (x) (adjoin-multiplier m x))
                                            (expansion (coeff t)))
                                       (iter (rest-terms term-list)))
                  (adjoin-monomial (adjoin-multiplier m (make-monomial (coeff t)))
                                   (iter (rest-terms term-list))))))))
    (iter (term-list p)))
  (define (rearrange monomiallist)
    (define (make-termlist target-multiplier to-term others)
      (if (null? others)
          (if (null? to-term)
              (the-empty-termlist)
              (adjoin-term (make-term (multiplier-power target-multiplier)
                                      (rearrange to-term))
                           (the-empty-termlist)))
          (let ((first (first-monomial others))
                (rest (rest-monomials others)))
            (cond ((multiplier=? target-multiplier (first-multiplier first))
                   (make-termlist target-multiplier
                                  (adjoin-monomial (rest-multipliers first)
                                                   to-term)
                                  rest))
                  ((variable=? (multiplier-variable target-multiplier)
                               (multiplier-variable (first-multiplier first)))
                   (adjoin-term (make-term (multiplier-power target-multiplier)
                                           (rearrange to-term))
                                (make-termlist (first-multiplier first)
                                               nil
                                               others)))
                  (else (adjoin-term (make-term (multiplier-power target-multiplier)
                                                (rearrange to-term))
                                     (make-termlist (make-multiplier (multiplier-variable target-multiplier)
                                                                     0)
                                                    others
                                                    nil)))))))
    (if (contains-variable? (first-monomial monomiallist))
        (let ((multiplier (first-multiplier (first-monomial monomiallist)))) 
          (make-poly (multiplier-variable multiplier)
                     (make-termlist multiplier nil monomiallist)))
        (first-monomial monomiallist)))
  ;; arithmetical operations
  (define (multiply-monomial-by-monomial m1 m2)
    (if (contains-variable? m1)
        (adjoin-multiplier (first-multiplier m1)
                           (multiply-monomial-by-monomial (rest-multipliers m1) m2))
        (adjoin-multiplier m1 m2)))
  (define (multiply-monomiallist-by-monomial monomial monomiallist)
    (if (empty-monomiallist? monomiallist)
        nil
        (adjoin-monomial (multiply-monomial-by-monomial monomial
                                                        (first-monomial monomiallist))
                         (multiply-monomiallist-by-monomial monomial
                                                            (rest-monomials monomiallist)))))
  
  (define (add-poly p1 p2)
    (rearrange (append-monomiallist (expansion p1)
                                    (expansion p2))))
  (define (mul-poly p1 p2)
    (define (mul-monomials m1 m2)
      (if (empty-monomiallist? m1)
          nil
          (append-monomiallist (multiply-monomiallist-by-monomial (first-monomial m1)
                                                                  m2)
                               (mul-monomials (rest-monomials m1)
                                              m2))))
    (mul-monomials (expansion p1) (expansion p2)))
  ;; interface for rest of the system
  (define (tag x) (attach-tag 'polynomial x))
  (put 'add '(polynomial polynomial) (lambda (x y) (add-poly (attach-tag 'polynomial x)
                                                                  (attach-tag 'polynomial y))))
  (put 'mul '(polynomial polynomial) (lambda (x y) (mul-poly (attach-tag 'polynomial x)
                                                                  (attach-tag 'polynomial y))))
  'multivariate-polynomial-package-installed)

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
(install-integer-package)
(install-rational-package)
(install-real-package)
(install-complex-package)
(install-multivariate-polynomial-package)
(install-raise-package)
(install-project-package)

(define p (make-poly 'x '(dense 1 0 2)))
(define p1 (make-poly 'x '(sparse (term 2
                                        (polynomial y sparse (term 1 1)
                                                              (term 0 1)))
                                  (term 1
                                        (polynomial y sparse (term 2 1)
                                                              (term 1 1)))
                                  (term 0
                                        (polynomial y sparse (term 2 1)
                                                              (term 1 1))))))
(define p2 (make-poly 'x '(sparse (term 1
                                        (polynomial y sparse (term 3 1)
                                                              (term 1 1))))))
(define p3 (make-poly 'y '(sparse (term 3
                                         (polynomial x sparse (term 0 1)))
                                   (term 1
                                         (polynomial x sparse (term 0 1))))))
                                 
(add p1 p2)
(add p2 p3)
(add p1 p3)
            