#lang sicp

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

; Procedures to work with tags
(define (attach-tag data-tag contents) (cons data-tag contents))
(define (data-tag datum) (car datum))
(define (contents datum) (cdr datum))

;; Department 1 package
(define (install-department1-package)
  (define data '((Nina (Appatch "89992348989" 90000))
                 (Katya (Igorevna "87280902233" 100000))))
  (define (get-name record) (car record))
  (define (get-record name)
    (define (get-record-iter name data)
      (if (null? data)
          false
          (let ((record (car data)))
            (if (eq? name (get-name record))
                record
                (get-record-iter name (cdr data))))))
    (get-record-iter name data))
  (define (get-salary record) (caddr (cadr record)))

  ; Interface
  (put 'get-record '(department1) get-record)
  (put 'get-salary '(department1) get-salary)
  'done)

;; Department 2 package 
(define (install-department2-package)
  (define data '((Ilya ((adress "Porg 283 h.24/5") (salary 95000)))
                 (Roman ((adress "Alp 89/5") (salary 120000)))))
  (define (get-name record) (car record))
  (define (get-record name)
    (define (get-record-iter name data)
      (if (null? data)
          false
          (let ((record (car data)))
            (if (eq? name (get-name record))
                record
                (get-record-iter name (cdr data))))))
    (get-record-iter name data))
  (define (get-salary record) (cadr (cadr (cadr record))))
  
  ; Interface
  (put 'get-record '(department2) get-record)
  (put 'get-salary '(department2) get-salary)
  'done)

; Generic procedures
(define (get-record name department)
  (let ((record ((get 'get-record (list department)) name)))
    (if record
        (attach-tag department record)
        false)))

(define (get-salary record)
  (let ((tag (data-tag record))
        (content (contents record)))
    ((get 'get-salary (list tag)) content)))

(define (find-employee-record name departments)
  (if (null? departments)
      false
      (let ((department (car departments)))
        (let ((record (get-record name department)))
          (if record
              record
              (find-employee-record name (cdr departments)))))))

; Testing
(install-department1-package)
(install-department2-package)

(get-record 'Nina 'department1)
(get-record 'Ilya 'department1)
(get-record 'Ilya 'department2)
(get-salary (get-record 'Nina 'department1))
(get-salary (get-record 'Ilya 'department2))

(find-employee-record 'Nina '(department2 department1))
 
