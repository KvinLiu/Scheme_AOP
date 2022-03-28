;; Exercise 3.1: sum
;; Define a procedure sum that finds the sum of the components of an n-tuple.
;; Test your procedure on:
;; (sum '(1 2 3 4 5)) ==> 15
(define (sum ntuple)
  (cond
   ((null? ntuple) 0)
   (else (+ (car ntuple) (sum (cdr ntuple))))))

;; Exercise 3.2: pairwise-sum
;; Define a procedure pairwise-sum that takes two n-tuples of the same length,
;; ntpl-1 and ntpl-2, as arguments and produces a new n-tuple whose components
;; are the sum of the corresponding components of ntpl-1 and ntpl-2.
;; Test your procedure on:
;; (pairwise-sum '(1 3 2) '(4 -1 2)) ==> (5 2 4)
(define (pairwise-sum ntpl1 ntpl2)
  (cond
   ((and (null? ntpl1)
         (null? ntpl2)) '())
   (else (cons (+ (car ntpl1) (car ntpl2))
               (pairwise-sum (cdr ntpl1) (cdr ntpl2))))))

(define (pairwise-product ntpl1 ntpl2)
  (cond
   ((null? ntpl1) '())
   (else (cons (* (car ntpl1) (car ntpl2))
               (pairwise-product (cdr ntpl1) (cdr ntpl2))))))


;; Exercise 3.3: dot-product
;; Define a procedure dot-product that takes two n-tuples of the same length,
;; multiplies the corresponding components, and adds the resulting products.
;; (dot-product '(3 4 -1) '(1 -2 -3)) ==> -2

(define dot-product
  (lambda (ntpl1 ntpl2)
    (sum (pairwise-product ntpl1 ntpl2))))

;; Exercise 3.4: mult-by-n
;; Define a procedure mult-by-n that takes a number num and an n-tuple ntpl as
;; arguments and multiplies each component of ntpl by num. Test your procedure
;; on:
;; (mult-by-n 3 '(1 2 3 4 5)) ==> (3 6 9 12 15)
(define (mult-by-n n lst)
  (cond
   ((null? lst) '())
   (else (cons (* n (car lst))
               (mult-by-n n (cdr lst))))))
