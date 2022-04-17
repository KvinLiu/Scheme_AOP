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
