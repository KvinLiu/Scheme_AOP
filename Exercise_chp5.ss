;; Exercise 5.1
;; Find the value of each of the following expressions, writing the local
;; environments for each of the nested let expressions. Draw arrows from
;; each variable to the parameter to which it is bound in a lambda or let
;; expression. Also draw an arrow from the parameter to the value to which
;; it is bound.
;; a.
(let ((a 5))                     ;;
  (let ((fun (lambda (x) (max x a)))) ;; max --> global, a --> upper a 5
    (let ((a 10)
          (r 20))
      (fun 1))))                 ;; fun -> lambda (x) (max x a), res --> 5

;; b.
(let ((a 1) (b 2))
  (let ((b 3) (c (+ a b)))       ;; a --> upper a 1, b --> upper b 2
    (let ((b 5))
      (cons a (cons b (cons c '())))))) ;; a -> 1, b -> 5, c -> 3,

;; Exercise 5.2
;; Find the value of each of the following letrec expressions:
;; a
(letrec
    ((loop (lambda (n k)
             (cond
              ((zero? k) n)
              ((< n k) (loop k n))
              (else (loop k (remainder n k)))))))
  (loop 9 12))  ;; -> (loop 12 9) -> (loop 9 3) -> (loop 3 0) -> 3

;; b
(letrec
    ((loop (lambda (n)
             (if (zero? n)
                 0
                 (+ (remainder n 10) (loop (quotient n 10)))))))
  (loop 1234)) ;; (+ 4 3 2 1 0) -> 10

;; Exercise 5.3
;; Write the two expression in Parts a and b of Exercise 5.1 as nested lambda expressions
;; without using any let expressions.
;; a
((lambda (x)
   ((lambda (x a) (max x a)) x 5)) 1)

;; b
(cons ((lambda (a) a) 1)
      (cons ((lambda (x) x) 5)
            (cons ((lambda (a b) (+ a b)) 1 2)
                  '())))

;; Exercise 5.4
;; Find the value of the following letrec expression.
(letrec ((mystery
          (lambda (tuple odds evens)
            (if (null? tuple)
                (append odds evens)
                (let ((next-int (car tuple)))
                  (if (odd? next-int)
                      (mystery (cdr tuple)
                               (cons next-int odds) evens)
                      (mystery (cdr tuple)
                               odds (cons next-int evens))))))))
  (mystery '(3 16 4 7 9 12 24) '() '())) ;; '(9 7 3 24 12 4 16)

;; Exercise 5.5
;; We define a procedure *mystery* as follows:
(define mystery
  (lambda (n)
    (letrec
        ((mystery-helper
          (lambda (n s)
            (cond
             ((zero? n) (list s))
             (else
              (append
               (mystery-helper (sub1 n) (cons 0 s))
               (mystery-helper (sub1 n) (cons 1 s))))))))
      (mystery-helper n '()))))
;; What is returned when (mystery 4) is invoked? Describe what is returned when
;; mystery is invoked with an arbitrary positive integer.
(mystery 4) ;; sort like print all 4 digits of binary.

;; Exercise 5.6: insert-left-all
;; Rewrite the definition of the procedure insert-left-all (See Exercise 4.6.)
;; using a locally defined procedure that takes the list ls as its only argument.
;;
;; (define (insert-left-all new old ls)
;;   (cond
;;    ((null? ls) '())
;;    ((pair? (car ls)) (cons (insert-left-all new old (car ls)) (insert-left-all new old (cdr ls))))
;;    ((equal? old (car ls)) (append (list new old) (insert-left-all new old (cdr ls))))
;;    (else (cons (car ls) (insert-left-all new old (cdr ls))))))
;;
;; (insert-left-all 'z 'a '(a ((b a)))) ==> (z a ((b z a)))
(define (insert-left-all new old ls)
  (letrec ((insert-local
            (lambda (ls*)
              (cond
               ((null? ls*) '())
               ((pair? (car ls*))
                (cons (insert-local (car ls*))
                      (insert-local (cdr ls*))))
               ((equal? old (car ls*))
                (append (list new old) (insert-local (cdr ls*))))
               (else (cons (car ls*) (insert-local (cdr ls*))))))))
    (insert-local ls)))

;; Exercise 5.7: fib
;; As in Program 5.4 for fact, write an iterative definition of fib using fib-it
;; (See Program 4.24.) as a local procedure
;;
(define (fib n)
  (letrec
      ((fib-it
        (lambda (n acc1 acc2)
          (if (= n 1)
              acc2
              (fib-it (sub1 n) acc2 (+ acc1 acc2))))))
    (fib-it n 0 1)))

;; Exercise 5.8: list-ref
;; Program 3.7 is a good definition of list-ref. Unfortunately, the information displayed
;; upon encountering a reference out of range is not as complete as we might expect. In
;; the definitions of list-ref, which precede it, however, adequate information is displayed.
;; Rewrite Program 3.7, using a letrec expression, so that adequate information is displayed.

;; Exercise 5.9
;; Implement the algebra of polynomials in the two ways indicated in the text.
;; For each implementation, test each of the procedures p+, p-, and p* with the
;; polynomials
;; P1(x) = 5x^4 - 7x^3 + 2x - 4
;; P2(x) = x^3 + 6x^2 - 3x
;; and using poly-value, find p1(-1), p1(2), p2(0), and p2(-2)
(define (zero-poly? poly)
  (and (zero? (degree poly)) (zero? (leading-coef poly))))

(define (p+ poly1 poly2)
  (cond
   ((zero-poly? poly1) poly2)
   ((zero-poly? poly2) poly1)
   (else (let ((n1 (degree poly1))
               (n2 (degree poly2))
               (a1 (leading-coef poly1))
               (a2 (leading-coef poly2))
               (rest1 (rest-of-poly poly1))
               (rest2 (rest-of-poly poly2)))
           (cond
            ((> n1 n2) (poly-cons n1 a1 (p+ rest1 poly2)))
            ((< n1 n2) (poly-cons n2 a2 (p+ poly1 rest2)))
            (else
             (poly-cons n1 (+ a1 a2)) (p+ rest1 rest2)))))))
(define p*
  (letrec
      ((t* (lambda (trm poly)
             (if (zero-poly? poly)
                 the-zero-poly
                 (poly-cons
                  (+ (degree trm) (degree poly))
                  (* (leading-coef trm) (leading-coef poly))
                  (t*  trm (rest-of-poly poly)))))))
    (lambda (poly1 poly2)
      (letrec
          ((p*-helper (lambda (p1)
                        (if (zero-poly? p1)
                            the-zero-poly
                            (p+ (t* (leading-term p1) poly2)
                                (p*-helper (rest-of-poly p1)))))))
        (p*-helper poly1)))))

;; Answer the two value is the same. different form have different selector.

;; Exercise 5.10
;; Look closely at the definition of p+ (see Program 5.9). When n1 is greater than
;; n2, the variable a2 and rest2 are ignored. Similarly, when n1 is less than n2,
;; the variables a1 and rest1 are ignored. Rewrite p+ so that this wasting of effort
;; disappears. Hint: You will need to use let within the consequents of cond clauses.

(define (np+ poly1 poly2)
  (let ((a (lambda (poly)
             (leading-coef poly)))
        (rest (lambda (poly)
                (rest-of-poly poly))))
   (cond
    ((zero-poly? poly1) poly2)
    ((zero-poly? poly2) poly1)
    (else
     (let ((n1 (degree poly1))
           (n2 (degree poly2)))
       (cond
        ((> n1 n2) (poly-cons n1 (a (poly1)) (np+ (rest poly1) poly2)))
        ((< n1 n2) (poly-cons n2 (a (poly2)) (np+ poly1 (rest poly2))))
        (else
         (poly-cons n1
                    (+ (a poly1) (a poly2))
                    (np+ (rest poly1) (rest poly2))))))))))

;; Exercise 5.11: poly-quotient, poly-remainder
;; Define a procedure poly-quotient that finds the quotient polynomial when poly1
;; is divided by poly2 and a procedure poly-remainder that finds the remainder
;; polynomial when poly1 is divided by poly2.
(define (poly-quotient poly1 poly2)
  (if (> (p- (poly1 poly2)) the-zero-poly)
      poly2
      (poly-quotient poly1 (p- poly2 poly1))))

;; Exercise 5.16
;; Convert each of the following decimal numbers to base 2.
;; a. 53
;; b. 404
(define decimal->binary
  (lambda (num)
    (letrec
        ((dec->bin
          (lambda (n deg)
            (if (zero? n)
                the-zero-poly
                (p+ (make-term deg (remainder n 2))
                    (dec->bin (quotient n 2) (add1 deg)))))))
      (poly->digits (dec->bin num 0)))))

;; Exercise 5.17
;; Convert each of the following base 2 numbers to decimals.
;; a. 10101010
;; b. 1101011

;; Exercise 5.18: octal->decimal, hexadecimal->decimal
;; Look over the programs for binary->decimal and decimal->binary and
;; see what changes have to be made to get definitions for the four
;; procedures:

;; Exercise 5.19: binary-sum, binary-product
;; Exercise 5.20: binary->decimal, decimal->binary
