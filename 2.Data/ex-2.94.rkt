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
             (let ((htag (highest-tag (unique data-tags)))) ; get tag with highest level in tag tower from unique data-tags
               (apply apply-generic (cons op (map (lambda (x) (raise-to htag x)) ; get raised to htag args
                                                  args)))))))))



;;;;; Generic arithmetical operations
(define (add . args) (apply apply-generic (cons 'add args)))
(define (sub . args) (apply apply-generic (cons 'sub args)))
(define (mul . args) (apply apply-generic (cons 'mul args)))
(define (div . args) (apply apply-generic (cons 'div args)))
(define (rem . args) (apply apply-generic (cons 'rem args)))
(define (greatest-common-divisor . args) (apply apply-generic (cons 'gcd args)))
(define (equ? . args) (apply apply-generic (cons 'equ? args)))
(define (neg x) (apply-generic 'neg x))
(define (exp x y) (apply-generic 'exp x y))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))
(define (=zero? x) (apply-generic '=zero? x))
(define (gcd x y) (apply-generic 'gcd x y))

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
  ;; initial procedures
  (define (gcd x y)
    (if (= y 0)
        x
        (gcd y (remainder x y))))
  
  ;; interface for rest of the system
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
  (put 'gcd '(integer integer) gcd) 
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
      (cons (div x g) (div y g))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
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
  ;; initial procedures
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  ;; interface for rest of the system
  (put 'gcd '(real real) gcd)
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
                     (display L1)
                     (display " ")
                     (display L2)
                     (newline)
                     (display c)
                     (newline)
                     (display (mul-termlist-by-term c L2))
                     (newline)
                     (display (sub-terms L1 (mul-termlist-by-term c L2)))
                     (newline)
                     (newline)
                     (cons (adjoin-term c (car rest-of-result))
                           (cdr rest-of-result)))))))))
  (define (remainder-terms a b)
    (cdr (div-terms a b)))
  (define (mul-termlist-by-term term terms)
    (if (empty-termlist? terms)
        (the-empty-termlist)
        (adjoin-term (make-term (add (order term) (order (first-term terms)))
                                (mul (coeff term) (coeff (first-term terms))))
                     (mul-termlist-by-term term (rest-terms terms)))))
  (define (gcd-terms a b)
    (display "gcd: ")
    (display a)
    (display " ")
    (display b)
    (newline)
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))))
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
  (define (gcd-poly p1 p2) ((operation-poly 'GCD-POLY gcd-terms) p1 p2))
  (define (add-poly p1 p2) ((operation-poly 'ADD-POLY add-terms) p1 p2))
  (define (mul-poly p1 p2) ((operation-poly 'MUL-POLY mul-terms) p1 p2))
  (define (sub-poly p1 p2) ((operation-poly 'SUB-POLY sub-terms) p1 p2))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((term-res (div-terms (term-list p1)
                                   (term-list p2))))
          (make-poly (variable p1)
                     (car term-res)))
        (error "Многочлены от разных переменных -- DIV-POLY" (list p1 p2))))
  (define (rem-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((term-res (div-terms (term-list p1)
                                   (term-list p2))))
          (make-poly (variable p1)
                     (cdr term-res)))
        (error "Многочлены от разных переменных -- REM-POLY" (list p1 p2))))
  ;; interface for rest of the system
  (define (tag x) (attach-tag 'polynomial x))
  (put 'variable '(polynomial) variable)
  (put 'term-list '(polynomial) term-list)
  (put '=zero? '(polynomial)
       (lambda (x) (empty-termlist? (term-list x))))
  (put 'neg '(polynomial)
       (lambda (p) (tag (neg-poly p))))
  (put 'equ? '(polynomial polynomial) eq-poly?)
  (put 'gcd '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'rem '(polynomial polynomial)
       (lambda (p1 p2) (tag (rem-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'polynomial-package-installed)

(define (make-poly variable terms)
  ((get 'make 'polynomial) variable terms))

;;; Multivariate polynomial package
(define (install-multivariate-polynomial-package)
  ;; dependency
  (install-polynomial-package)
  ;; import from dependency packages
  (define make-term (get 'make 'term))
  (define (order x) (apply-generic 'order x))
  (define (coeff x) (apply-generic 'coeff x))
  (define (first-term x) (apply-generic 'first-term x))
  (define (rest-terms x) (apply-generic 'rest-terms x))
  (define (term-list x) (apply-generic 'term-list x))
  (define (empty-termlist? x) (apply-generic 'empty-termlist? x))
  (define the-empty-termlist (get 'the-empty-termlist 'sparse))
  (define (variable x) (apply-generic 'variable x))
  (define (adjoin-term x y) (apply-generic 'adjoin-term x y))
  ;; innternal procedures
  (define (variable=? v1 v2)
    (eq? v1 v2))
  (define (variable<? v1 v2)
    (define valid-variables '(x y z t))
    (define (iter vars)
      (cond ((null? vars)
             (error "Недопустимая перемнная -- VALID-VARIABLES" (list v1 v2)))
            ((variable=? (car vars) v1) true)
            ((variable=? (car vars) v2) false)
            (else (iter (cdr vars)))))
    (iter valid-variables))
  ; multiplier abstraction
  (define (make-mult var order)
    (list var order))
  (define (mult-variable? mult)
    (pair? mult))
  (define (mult-variable mult)
    (car mult))
  (define (mult-order mult)
    (cadr mult))
  (define (mult=? m1 m2)
    (if (and (mult-variable? m1) (mult-variable? m2))
        (and (variable=? (mult-variable m1) (mult-variable m2))
             (= (mult-order m1) (mult-order m2)))
        (and (not (mult-variable? m1)) (not (mult-variable? m2))))) ; multipliers 7 and 8 considers equal
  (define (mult>? m1 m2)
    (cond ((and (mult-variable? m1) (mult-variable? m2))
           (if (variable=? (mult-variable m1) (mult-variable m2))
               (> (mult-order m1) (mult-order m2))
               (variable<? (mult-variable m1) (mult-variable m2))))
          ((mult-variable? m1) true)
          ((mult-variable? m2) false)
          (else false)))
  ; monomial abstraction
  (define (make-monom mult)
    (list mult))
  (define (first-mult monom)
    (car monom))
  (define (rest-mults monom)
    (cdr monom))
  (define (contains-variable? monom)
    (mult-variable? (first-mult monom)))
  (define (number-of-mults monom)
    (length monom))
  (define (empty-monom? monom) (null? monom))
  (define (the-empty-monom) nil)
  (define (factor monom)
    (if (contains-variable? monom)
        (factor (rest-mults monom))
        (first-mult monom)))
  (define (add-factor monom factor)
    (if (mult-variable? (first-mult monom))
        (cons (first-mult monom)
              (add-factor (rest-mults monom)
                          factor))
        (list (add (first-mult monom) factor))))
  (define (monom=? m1 m2)
    (if (= (number-of-mults m1) (number-of-mults m2))
        (if (= (number-of-mults m1) 0)
            true
            (and (mult=? (first-mult m1) (first-mult m2))
                 (monom=? (rest-mults m1) (rest-mults m2))))
        false))
  (define (monom>? m1 m2)
    (cond ((and (contains-variable? m1) (contains-variable? m2))
           (if (mult=? (first-mult m1) (first-mult m2))
               (monom>? (rest-mults m1) (rest-mults m2))
               (mult>? (first-mult m1) (first-mult m2))))
          ((contains-variable? m1) true)
          ((contains-variable? m2) false)
          (else false)))
  (define (adjoin-mult mult monom)
    (cond ((and (mult-variable? mult) (=zero? (mult-order mult))) monom)
          ((and (not (mult-variable? mult)) (not (contains-variable? monom))) ; both are factors
           (make-monom (mul mult (first-mult monom))))
          ((not (mult-variable? mult)) ; multiplier is a factor and monomial contains variable
           (cons (first-mult monom) (adjoin-mult mult (rest-mults monom))))
          ((not (contains-variable? monom)) ; monomial is one factor without variables and mltiplier is a variable
           (cons mult monom))
          (else (let ((first (first-mult monom))
                      (rest (rest-mults monom)))
                  (cond ((variable=? (mult-variable mult) (mult-variable first))
                         (cons (make-mult (mult-variable mult)
                                          (+ (mult-order mult) (mult-order first)))
                               rest))
                        ((variable<? (mult-variable mult) (mult-variable first))
                         (cons mult monom))
                        (else (cons (first-mult monom)
                                    (adjoin-mult mult rest))))))))
  ; monom-list abstraction
  (define (first-monom monom-list)
    (car monom-list))
  (define (rest-monoms monom-list)
    (cdr monom-list))
  (define (empty-monomlist? monom-list)
    (null? monom-list))
  (define (the-empty-monomlist) nil)
  (define (adjoin-monom monom monom-list)
    (if (empty-monomlist? monom-list)
        (list monom)
        (let ((first (first-monom monom-list))
              (rest (rest-monoms monom-list)))
          (cond ((monom=? monom first)
                 (cons (add-factor monom (factor first))
                       rest))
                ((monom>? monom first)
                 (cons monom monom-list))
                (else (cons first (adjoin-monom monom rest)))))))
  (define (append-monomlist m1 m2)
    (if (empty-monomlist? m1)
        m2
        (append-monomlist (rest-monoms m1)
                          (adjoin-monom (first-monom m1)
                                        m2))))
  ; expansion procedures
  (define (expansion-term var term)
    (let ((mult (make-mult var
                           (order term))))
      (map (lambda (x)
             (adjoin-mult mult x))
           (expansion (coeff term)))))
  (define (expansion-termlist var term-list)
    (if (empty-termlist? term-list)
        (the-empty-monomlist)
        (append-monomlist (expansion-term var (first-term term-list))
                          (expansion-termlist var (rest-terms term-list)))))
  (define (expansion p)
    (if (eq? (data-tag p) 'polynomial)
        (expansion-termlist (variable p) (term-list p))
        (list (make-monom p))))
  ; rearrange procedures
  (define (split var monom-list)
    (define (iter m1 m2)
      (cond ((empty-monomlist? m2) (cons m1 m2))
            ((mult=? (first-mult (first-monom m1)) (first-mult (first-monom m2)))
             (iter (adjoin-monom (first-monom m2) m1) (rest-monoms m2)))
            (else (cons m1 m2))))
    (let ((m (first-mult (first-monom monom-list))))
      (if (and (mult-variable? m) (variable=? var (mult-variable m)))
          (iter (list (first-monom monom-list)) (rest-monoms monom-list))
          (cons monom-list '()))))

  (define (term-from-monomlist ord monom-list)
    (make-term ord
               (rearrange monom-list)))
  (define (termlist-from-monomlist var monom-list)
    (if (empty-monomlist? monom-list)
        (the-empty-termlist)
        (let ((d (split var monom-list)))
          (let ((m (first-mult (first-monom (car d)))))
            (if (and (mult-variable? m) (variable=? var (mult-variable m)))
                (adjoin-term (term-from-monomlist (mult-order m)
                                                  (map (lambda (x) (rest-mults x)) (car d)))
                             (termlist-from-monomlist var (cdr d)))
                (adjoin-term (term-from-monomlist 0 (car d))
                             (the-empty-termlist)))))))
  (define (rearrange monom-list)
    (if (pair? monom-list) ; monom-list
        (let ((m (first-mult (first-monom monom-list))))
          (if (mult-variable? m)
              (make-poly (mult-variable m)
                         (termlist-from-monomlist (mult-variable m)
                                                  monom-list))
              m))
        monom-list)) ; number
  ; arithmetical operations
  (define (add-poly p1 p2)
    (rearrange (append-monomlist (expansion p1)
                                 (expansion p2))))
  (define (mul-monom-by-monom m1 m2)
    (fold-right m2
                (lambda (mult monom) (adjoin-mult mult monom))
                m1))
  (define (mul-monomlist-by-monom monom monom-list)
    (fold-left (the-empty-monomlist)
               (lambda (res x) (adjoin-monom (mul-monom-by-monom monom x) res))
               monom-list))
  (define (mul-monomlist-by-monomlist m1 m2)
    (fold-left (the-empty-monomlist)
               (lambda (res x) (append-monomlist (mul-monomlist-by-monom x m2) res))
               m1))
  (define (mul-poly p1 p2)
    (rearrange (mul-monomlist-by-monomlist (expansion p1)
                                           (expansion p2))))
  ;; interface for rest of the program
  (define (tag x) (attach-tag 'polynomial x))
  (put 'add '(polynomial polynomial) (lambda (x y) (add-poly (tag x) (tag y))))
  (put 'mul '(polynomial polynomial) (lambda (x y) (mul-poly (tag x) (tag y))))
  (put 'expansion 'test expansion)
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

(define p1 (make-poly 'x '(sparse (term 4 1) (term 3 -1) (term 2 -2) (term 1 2))))
(define p2 (make-poly 'x '(sparse (term 3 1) (term 1 -1))))

(greatest-common-divisor p1 p2)
