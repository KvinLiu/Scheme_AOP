;; Program 3.1: add1
(define add1
  (lambda (n)
    (+ n 1)))

;; Program 3.2: sub2
(define (sub1 n)
  (- n 1))

;; Program 3.4: harmonic-sum
(define (harmonic-sum n)
  (cond
   ((zero? n) 0)
   (else (+ (/ 1 n) (harmonic-sum (sub1 n))))))

;; Program 3.5: list-of-zeros
(define list-of-zeros
  (lambda (n)
    (cond
     ((zero? n) '())
     (else (cons 0 (list-of-zeros (sub1 n)))))))

;; Program 3.6: length
(define =length=
  (lambda (lst)
    (cond
     ((null? lst) 0)
     (else (add1 (length (cdr lst)))))))

;; Program 3.7: list-ref
(define (list-ref ls n)
  (cond
   ((null? ls)
    (error "list-ref: Index " n "out of range for list" ls))
   ((zero? n) (car ls))
   (else (list-ref (cdr ls) (sub1 n)))))

;; Program 3.8: rzero?
(define rzero?
  (lambda (rtl)
    (zero? (numr rtl))))

;; Program 3.9: r+
(define r+
  (lambda (x y)
    (make-ratl
     (+ (* (numr x) (denr y))
        (* (numr y) (denr x)))
     (* (denr x) (denr y)))))

;; Program 3.10: r*
(define (r* x y)
  (make-ratl
   (* (numr x) (numr y))
   (* (denr x) (denr y))))

;; Program 3.11: r-
(define (r- x y)
  (make-ratl
   (- (* (numr x) (denr y)) (* (numr y) (denr x)))
   (* (denr x) (denr y))))

;; Program 3.12: rinvert
(define (rinvert rtl)
  (if (rzero? rtl)
      (error "rinvert: Cannot invert" rtl)
      (make-ratl (denr rtl) (numr rtl))))

;; Program 3.13: r/
(define (r/ x y)
  (r* x (rinvert y)))

;; Program 3.14: r=
(define (r= x y)
  (= (* (numr x) (denr y))
     (* (numr y) (denr x))))

;; Program 3.15: rpositive?
(define (rpositive? rtl)
  (or (and (positive? (numr rtl)) (positive? (denr rtl)))
      (and (negative? (numr rtl)) (negative? (denr rtl)))))

;; Program 3.16: r>
(define (r> x y)
  (rpositive? (r- x y)))

;; Program 3.17: max
(define (max x y)
  (if (x > y)
      x
      y))

;; Program 3.18: rmax
(define (rmax x y)
  (if (r> x y)
      x
      y))

;; Program 3.19: extreme-value
(define extreme-value
  (lambda (pred x y)
    (if (pred x y)
        x
        y)))
