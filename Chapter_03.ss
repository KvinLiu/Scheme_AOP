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
    (error: "list-ref: Index " n "out of range for list" ls))
   ((zero? n) (car ls))
   (else (list-ref (cdr ls) (sub1 n)))))
