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

;; Exercise 3.5: index
;; Define a procedure index that has two arguments, and item a and a list of
;; items ls, and returns the index of a in ls, that is, the zero-based location
;; of a in ls. If the item is not in the list, the procedure returns -1.
;; (index 3 '(1 2 3 4 5 6)) ==>

(define index-helper
  (lambda (n lst proceed)
    (cond
     ((null? lst) -1)
     ((equal? n (car lst)) proceed)
     (else (index-helper n (cdr lst) (add1 proceed))))))

(define index
  (lambda (n lst)
    (index-helper n lst 0)))

;; Exercise 3.6: make-list
;; Define a procedure make-list that takes as arguments a nonnegative integer num
;; and an item a and return a list of num elements, each of which is a. Test your
;; procedure on:
;; (make-list 5 'no) ==> (no no no no no)
;; (length (make-list 7 'any)) ==> 7
;; (all-same? (make-list 100 'any)) ==> #t
(define (make-list n item)
  (cond
   ((zero? n) '())
   (else (cons item (make-list (sub1 n) item)))))

;; Exercise 3.7: count-background
;; Define a procedure count-background that takes an item a and a list of items ls
;; as arguments and returns the number of items in ls that are not equal? to a.
;; Test your procedure on:
;; (count-background 'blue '(red white blue yellow blue red)) ==> 4
(define (count-background a lst)
  (cond
   ((null? lst) 0)
   ((equal? a (car lst)) (count-background a (cdr lst)))
   (else (add1 (count-background a (cdr lst))))))

;; Exercise 3.8: list-front
;; Define a procedure list-front that takes as arguments a list of items ls and a
;; nonnegative integer num and returns the first num top-level items in ls. If num
;; is larger than the number of top-level items in ls, an error is signaled. Test
;; your procedure on:
;; (list-front '(a b c d e f g) 4) => (a b c d)
(define (list-front lst num)
  (cond
   ((< (length lst) num) (error "Error: length of " lst "is less than " num))
   ((zero? num) '())
   (else (cons (car lst) (list-front (cdr lst) (sub1 num))))))

;; Exercise 3.9: wrapa
;; Define a procedure wrapa that takes as arguments an item a and a nonnegative
;; integer num and wraps num sets of parentheses around the item a. Test your
;; procedure on:
;; (wrapa 'gift 1) => (gift)
;; (wrapa 'sandwich 2) => ((sandwich))
(define (wrapa item num)
  (cond
   ((zero? num) item)
   (else (cons (wrapa item (sub1 num)) '()))))

;; Exercise 3.10: multiple?
;; Define a predicate multiple? that takes as arguments two integers m and n and
;; retturns #t if m is an integer multiple of n. (Hint: Use remainder.) Test your
;; procedure on:
;;(multiple? 7 2) ==> #f
;;(multiple? 9 3) ==> #t
(define (multiple? m n)
  (cond
   ((and (zero? m) (zero? n)) #t)
   ((and (not (zero? m)) (zero? n)) #f)
   (else (zero? (remainder m n)))))

;; Exercise 3.11: sum-of-odds
;; It can be shown that the sum of the first n odd numbers is equal to n^2. For
;; example,
;; 1 + 3 + 5 + 7 = 16 = 4^2
(define (helper n)
  (cond
   ((zero? n) '())
   ((odd? (sub1 (* 2 n))) (cons (sub1 (* 2 n)) (helper (sub1 n))))
   (else (helper (sub1 n)))))

(define (sum-of-lst lst)
  (cond
   ((null? lst) 0)
   (else (+ (car lst) (sum-of-lst (cdr lst))))))

(define sum-of-odds
  (lambda (n)
    (sum-of-lst (helper n))))

;; Exercise 3.12 n-tuple->integer
;; Define a procedure n-tuple->integer that converts a nonempty n-tuple of digits into
;; the number having those digits. Test your program on the following:
;; (n-tuple->integer '(3 1 4 6)) ==> 3146
(define (n-tuple->integer tup)
  (cond
   ((null? tup) (error "bad argument " tup "to n-tuple->integer"))
   ((null? (cdr tup)) (car tup))
   (else (+ (* (car tup) (expt 10 (sub1 (length tup))))
            (n-tuple->integer (cdr tup))))))
