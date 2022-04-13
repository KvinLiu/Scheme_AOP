;; Exercise 4.1: insert-left
;; Define a procedure insert-left with parameters new, old, and ls that builds
;; a list obtained boy inserting the item new to the left of each top-level
;; occurrence of the item old in the list ls. Test with:
;; (insert-left 'z 'a '(a b a c a)) ==> (z a b z a c z a)
(define (insert-left new old ls)
  (cond
   ((null? ls) '())
   ((equal? old (car ls)) (append (list new old) (insert-left new old (cdr ls))))
   (else (cons (car ls) (insert-left new old (cdr ls))))))

;; Exercise 4.2: insert-right
;; Define a procedure insert-right with parameters new, old, and ls that builds a
;; list obtained by inserting the item new to the right of each top-level occurrence
;; of the item old in the list ls. Test with:
;; (insert-right 'z 'a '(a b a c a)) ==> (a z b a z c a z)
(define (insert-right new old ls)
  (cond
   ((null? ls) '())
   ((equal? old (car ls)) (append (list old new) (insert-right new old (cdr ls))))
   (else (cons (car ls) (insert-right new old (cdr ls))))))

;; Exercise 4.3: subst
;; Define a procedure subst with parameters new, old, and ls that builds a list
;; obtained by replacing each top-level occurrence of the item old in the list ls
;; by the new. Test with:
;; (subst 'z 'a '(a b a c a)) ==> (z b z c z)
(define (subst new old ls)
  (cond
   ((null? ls) '())
   ((equal? old (car ls)) (cons new (subst new old (cdr ls))))
   (else (cons (car ls) (subst new old (cdr ls))))))

;; Exercise 4.4: deepen-1
;; Define a procedure deepen-1 with parameter ls that wraps a pair of parentheses
;; around each top-level item in ls. Test with:
;; (deepen-1 '(a b c d)) ==> ((a) (b) (c) (d))
;; (deepen-1 '((a b) (c (d e)) f)) ==> (((a b)) ((c (d e))) (f))
(define (deepen-1 lst)
  (cond
   ((null? lst) '())
   (else (cons (list (car lst)) (deepen-1 (cdr lst))))))

;; Exercise 4.5: subst-all, substq-all
;; Define a procedure subst-all with call structure (subst-all new old ls) that replaces
;; each occurrence of the item old in a list ls with the item new.
;; Test with:
;; (subst-all 'z 'a '(a (b (a c)) (a (d a)))) ==> (z (b (z c)) (z (d z)))
(define (subst-all new old ls)
  (cond
   ((null? ls) '())
   ((equal? old (car ls)) (cons new (subst-all new old (cdr ls))))
   ((pair? (car ls)) (cons (subst-all new old (car ls)) (subst-all new old (cdr ls))))
   (else (cons (car ls) (subst-all new old (cdr ls))))))

(define (substq-all new old ls)
  (cond
   ((null? ls) '())
   ((pair? (car ls)) (cons (substq-all new old (car ls)) (substq-all new old (cdr ls))))
   ((eq? old (car ls)) (cons new (substq-all new old (cdr ls))))
   (else (cons (car ls) (substq-all new old (cdr ls))))))

;; Exercise 4.6: insert-left-all
;; Define a procedure insert-left-all with call structure (insert-left-all new old ls)
;; that inserts the item new to the left of each occurrence of the item old in the ls.
;; Test with:
;; (insert-left-all 'z 'a '(a ((b a)))) ==> (z a ((b z a) ((z a (c)))))
(define (insert-left-all new old ls)
  (cond
   ((null? ls) '())
   ((pair? (car ls)) (cons (insert-left-all new old (car ls)) (insert-left-all new old (cdr ls))))
   ((equal? old (car ls)) (append (list new old) (insert-left-all new old (cdr ls))))
   (else (cons (car ls) (insert-left-all new old (cdr ls))))))

;; Exercise 4.7: sum-all
;; Define a procedure sum-all that finds the sum of the numbers in a list that may contain
;; nested sublists of numbers.
;; Test with:
;; (sum-all '((1 3) (5 7) (9 11))) ==> 36
(define (sum-all ls)
  (cond
   ((null? ls) 0)
   ((pair? (car ls)) (+ (sum-all (car ls)) (sum-all (cdr ls))))
   (else (+ (car ls) (sum-all (cdr ls))))))

;; Exercise 4.8: count-parens-all
;; Write the definition of a procedure count-parens-all that takes a list as its argument
;; and counts the number of opening and closing parentheses in the list. Test with:
;; (count-parens-all '()) ==> 2
(define (count-parens-all ls)
  (cond
   ((null? ls) 2)
   ((or (pair? (car ls))
        (null? (car ls)))
    (+ (count-parens-all (car ls)) (count-parens-all (cdr ls))))
   (else
    (+ 0 (count-parens-all (cdr ls))))))

;; Exercise 4.9: count-background-all
;; Define a procedure count-background-all that takes as its arguments item and a list ls
;; and return the number of items in ls that are not the same as item. Use the appropriate
;; sameness predicate for the data shown in the examples. Test with
;; (count-background-all 'a '((a) b (c a) d)) ==> 3
(define (count-background-all item ls)
  (cond
   ((null? ls) 0)
   ((pair? (car ls)) (+ (count-background-all item (car ls)) (count-background-all item (cdr ls))))
   ((or (equal? (car ls) item)
        (null? (car ls))) (count-background-all item (cdr ls)))
   (else (add1 (count-background-all item (cdr ls))))))

;; Exercise 4.10: leftmost
;; Define a procedure leftmost that takes a nonempty list as its argument and returns the
;; leftmost atomic item in the list. Test with:
(define (leftmost ls)
  (cond
   ((pair? (car ls)) (leftmost (car ls)))
   ((atom? (car ls)) (car ls))
   (else (leftmost (cdr ls)))))

;; Exercise 4.11: rightmost
;; Define a procedure rightmost that takes a nonempty list as its argument and returns the
;; rightmost atomic item in the list. Test with:
(define (rightmost ls)
  (if (null? (cdr ls))
      (if (atom? (car ls))
          (car ls)
          (rightmost (car ls)))
      (rightmost (cdr ls))))

(define (rightmost2 ls)
  (cond
   ((null? (cdr ls))
    (if (atom? (car ls))
        (car ls)
        (rightmost (car ls))))
   (else (rightmost (cdr ls)))))

;; Exercise 4.12
;; Enter the procedure fact into the computer and compute (fact n) for n = 10, 20, 30, 40,
;; 50 and 100. You will have an opportunity to observe how the implementation of Scheme you
;; are using displays large numbers.
(define (fact n)
  (if (zero? n)
      1
      (* n (fact (sub1 n)))))

(define (fact-it n acc)
  (if (zero? n) 
      acc
      (fact-it (sub1 n) (* n acc))))

;; Exercise 4.13
;; What happens when you invoke (fact 3.5)?
;; cause there is no result can met perdiate zero? the fn freeze or runs forever.

;; Exercise 4.14: harmonic-sum-it
;; Define an iterative procedure harmonic-sum-it that sums the first n terms of the harmonic
;; series

;; Get help function from Program 3.8: rzero? 3.9: r+ 3.21
(define (r+ x y)
  (make-ratl 
    (+ (* (numr x) (denr y)) (* (numr y) (denr x)))
    (* (denr x) (denr y))))

(define (numr rtl)
  (car rtl))

(define (denr rtl)
  (cadr rtl))

(define (rzero? rtl)
  (zero? (numr rtl)))

(define (make-ratl int1 int2)
  (if (zero? int2)
      (error "make-ratl: The denominator cannot be zero.")
      (cons (/ int1 (gcd int1 int2))
            (/ int2 (gcd int1 int2)))))

(define (harmonic-sum-it n acc)
  (if (zero? n)
      acc
      (harmonic-sum-it 
        (sub1 n) 
        (r+ (make-ratl 1 (sub1 n)) acc))))

;; Exercise 4.15
;; Rewrite the recursive version of the procedure fib with the line
;; (writeln "n = " n)
;; inserted just below the line (lambda (n). Then compute (fib 4) and compare the results with
;; the tree in. Also compute (fib 5) and (fib 6) observe how the number of recursive calls to
;; fib increases.
(define (fib n)
  ((writeln "n = " n)
   (if (zero? n)
       1
       (+ (fib (- n 1)) (fib (- n 2))))))

;; Exercise 4.16
;; Rewrite the iteractive version of the procedure fib-it with the line
;; (writeln "n = " n ", acc1 = " acc1 ", acc2 = " acc2)
;; inserted just below the line
;; (lambda (n acc1 acc2))

;; Exercise 4.17: calls-fib, adds-fib
;; Write the definitions of the procedures calls-fib and adds-fib discussed in this section.

;; Exercise 4.18: length-it
;; Write an iterative version length-it of the procedure length that computes the length of
;; a list.
(define (length-it ls acc)
  (if (null? ls)
      acc
      (length-it (cdr ls) (add1 acc))))
