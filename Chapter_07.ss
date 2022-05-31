;; Program 7.1 map
(define (map proc ls)
  (if (null? ls)
      '()
      (cons (proc (car ls)) (map proc (cdr ls)))))

;; Program 7.2 for-each
(define (for-each proc ls)
  (if (not (null? ls))
      (begin
        (proc (car ls))
        (for-each proc (cdr ls)))))

;; Program 7.3 add
(define add
  (letrec ((list-add
            (lambda (ls)
              (if (null? ls)
                  0
                  (+ (car ls) (list-add (cdr ls)))))))
    (lambda args
      (list-add args))))

;; Program 7.4 list
(define list (lambda args args))

;; Program 7.5 writln
(define (writeln args)
  (for-each display args)
  (newline))

;; Program 7.6 error
(define (error args)
  (display "Error: ")
  (for-each
   (lambda (value)
     (display " ")
     (display value))
   args)
  (newline)
  (reset))

;; Program 7.7 add
(define (add args)
  (if (null? args)
      0
      (+ (car args) (apply add (cdr args)))))

;; Program 7.8 compose
(define (compose f g)
  (lambda (x)
    (f (g x))))

;; Program 7.9 plus
(define (plus x y)
  (if (zero? y)
      x
      (add1 (plus x (sub1 y)))))

;; Program 7.10 times
(define (times x y)
  (if (zero? y)
      0
      (plus x (times x (sub1 y)))))

;; Program 7.11 exponent
(define (exponent x y)
  (if (zero? y)
      1
      (times x (exponent x (sub1 y)))))

;; Program 7.12 super
(define (super x y)
  (if (zero? y)
      1
      (exponent x (super x (sub1 y)))))

;; Program 7.13 superduper
(define (superduper x y)
  (if (zero? y)
      1
      (super x (superduper x (sub1 y)))))

;; Program 7.14 super-order
(define (super-order n)
  (cond
   ((= n 1) plus)
   ((= n 2) times)
   (else (lambda (x y)
           (cond
            ((zero? y) 1)
            (else ((super-order (sub1 n))
                   x
                   ((super-order n) x (sub1 y)))))))))

;; Program 7.15 ackermann
(define (ackermann n)
  ((super-order n) n n))

;; Program 7.16 member?-c
(define (member?-c item)
  (letrec
      ((helper (lambda (ls)
                 (if (null? ls)
                     #f
                     (or (equal? (car ls) item)
                         (helper (cdr ls)))))))
    helper))

;; Program 7.17 apply-to-all
(define (apply-to-all proc)
  (letrec
      ((helper
        (lambda (ls)
          (if (null? ls)
              '()
              (cons (proc (car ls)) (helper (cdr ls)))))))
    helper))

;; Program 7.18 sum
(define sum
  (letrec
      ((helper
        (lambda (ls)
          (if (null? ls)
              0
              (+ (car ls) (helper (cdr ls)))))))
    helper))

;; Program 7.19 product
(define product
  (letrec
      ((helper
        (lambda (ls)
          (if (null? ls)
              1
              (* (car ls) (helper (cdr ls)))))))
    helper))

;; Program 7.20 swapper-m
(define (swapper-m x y)
  (letrec
      ((helper
        (lambda (ls)
          (cond
           ((null? ls) '())
           ((equal? (car ls) x) (cons y (helper (cdr ls))))
           ((equal? (car ls) y) (cons x (helper (cdr ls))))
           (else (cons (car ls) (helper (cdr ls))))))))
    helper))
