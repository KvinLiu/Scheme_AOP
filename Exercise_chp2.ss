;; Exercise 2.1: second
(define (second ls)
  (cadr ls))

;; (define second
;;   (lambda (ls)
;;     (car (cdr ls))))

;; Exercise 2.2: third
(define (third ls)
  (caddr ls))

;; (define third
;;   (lambda (ls)
;;     (car (cdr (cdr ls)))))

;; Exercise 2.3: firsts-of-both
(define firsts-of-both
  (lambda (list-1 list-2)
    (make-list-of-two (car list-1) (car list-2))))
(define make-list-of-two
  (lambda (one two)
    (cons one (cons two '()))))
;; a
;; '(1 2)

;; b
;; '((a b) (e f))

;; Exercise 2.4: juggle
(define juggle
  (lambda (lst3)
    (cons
     (third lst3)
     (firsts-of-both lst3 (cdr lst3)))))

;; Exercis 2.5: switch
(define switch
  (lambda (lst3)
    (cons
     (third lst3)
     (firsts-of-both (cdr lst3) lst3))))

;; Exercise 2.6
;; Assume that a, b, and c are expressions that evaluate to #t and that e and f
;; are expressions that evaluate to #f. Decide whether the following expressions
;; are true or false
;;; a
(and a (or b e)) ;; #t
;;; b
(or e (and (not f) a c)) ;; #t
;;; c
(not (or (not a) (not b))) ;; #t
;;; d
(and (or a f) (not (or b e))) ;; #f

;; Exercise 2.7
;; Decide whether the following expressions are true or false if expr is some
;; boolean expression.
;;; a
(or (symbol? expr) (not (symbol? expr))) ;; #t
;;; b
(and (null? expr) (not (null? expr))) ;; #f
;;; c
(not (and (or expr #f) (not expr))) ;; #t
;;; d
(not (or expr #t)) ;; #f

;; Exercise 2.8
;; Decide whether the following expressions are true or false using s-and-n-list?
;; as defined in this section.
(define s-and-n-list?
  (lambda (ls)
    (and
     (pair? ls)
     (symbol? (car ls))
     (pair? (cdr ls))
     (number? (cadr ls)))))
;;; a
(s-and-n-list? '(2 pair 12 dozen)) ;; #f
;;; b
(s-and-n-list? '(b 4 u c a j)) ;; #t
;;; c
(s-and-n-list? '(a ten)) ;; #f
;;; d
(s-and-n-list? (cons 'b '())) ;; #f ?? error ??

;; Exercise 2.9
;; Decide whether the following expressions are true or false using s-or-n-list?
;; as defined in this section.
(define s-or-n-list?
  (lambda (ls)
    (and
     (pair? ls)
     (or (symbol? (car ls))
         (number? (car ls))))))
;;; a
(s-or-n-list? '(b)) ;; #t
;;; b
(s-or-n-list? '(c 2 m)) ;; #t
;;; c
(s-or-n-list? '(10 10 10 10)) ;; #t
;;; d
(s-or-n-list? '()) ;; #f

;; Exercise 2.10
;; Rewrite the definitions of the three procedures last-item, member? and remove-1st
;; with the cond expression replaced by if expressions.
(define (last-item lst)
  (if (null? (cdr lst))
      (car lst)
      (last-item (cdr lst))))

(define (member? item lst)
  (if (null? lst)
      #f
      (or (equal? item (car lst))
          (member? item (cdr lst)))))

(define (remove-1st item lst)
  (if (null? lst)
      '()
      (if (equal? item (car lst))
          (cdr lst)
          (cons (car lst) (remove-1st item (cdr lst))))))

;; Exercise 2.11
;; The definition of member? given in this section uses an or expression in the else
;; clause. Rewrite the definition of member? so that each of the two subexpressions of
;; the or expression is handled in a separate cond clause. Compare the resulting definition
;; with the definition of remove-1st
(define (member-c? a lat)
  (cond
   ((null? lat) #f)
   ((eq? a (car lat)) #t)
   (else (member-c? a (cdr lat)))))

;; Exercise 2.12
;; mystery, it takes a at least two items list and remove the last item of the list.
(define mystery
  (lambda (ls)
    (if (null? (cddr ls))
        (cons (car ls) '())
        (cons (car ls) (mystery (cdr ls))))))
;; new proper name remove-last

;; Exercise 2.13: subst-1st
(define subst-1st
  (lambda (a b ls)
    (cond
     ((null? ls) #f)
     ((equal? b (car ls)) (cons a (cdr ls)))
     (else (cons (car ls) (subst-1st a b (cdr ls)))))))

;; Exercise 2.14: insert-right-1st
;; The procedure insert-right-1st is like remove-1st except that instead of removing the item
;; that it is searching for, it inserts a new item to its right.
(define (insert-right-1st new old ls)
  (cond
   ((null? ls) '())
   ((equal? old (car ls))
    (cons old (cons new (cdr ls))))
   (else (cons (car ls) (insert-right-1st new old (cdr ls))))))

(define (insert-left-1st new old ls)
  (cond
   ((null? ls) '())
   ((equal? old (car ls))
    (cons new ls))
   (else (cons (car ls) (insert-left-1st new old (cdr ls))))))

;; Exercise 2.15: list-of-first-items
;; Define a procedure list-of-first-items that takes as its argument a list composed of
;; nonempty lists of items. Its value is a list composed of the first top-level item in
;; each of the sublists. Test your procedure on:
;; (list-of-first-items '((a) (b c d) (e f))) ==> (a b e)
(define (list-of-first ls)
  (cond
   ((null? ls) '())
   ((null? (car ls)) (list-of-first (cdr ls)))
   (else (cons (caar ls) (list-of-first (cdr ls))))))

;; Exercise 2.16: replace
;; Define a procedure replace that replaces each top-level item in a list of items ls by
;; a given item new-item. Test your procedure on:
;; (replace 'no '(will you do me a favor))
(define (replace item ls)
  (cond
   ((null? ls) '())
   (else (cons item (replace item (cdr ls))))))

;; Exercise 2.17: remove-2nd
;; Define a procedure remove-2nd that removes the second occurrence of a given item a form
;; a list of item ls. You may use the procedure remove-1st in defining remove-2nd.
;;(remove-2nd 'cat '(my cat loves cat food))
(define (remove-2nd item ls)
  (cond
   ((null? ls) '())
   ((equal? (car ls) item) (cons (car ls) (remove-1st item (cdr ls))))
   (else (cons (car ls) (remove-2nd item (cdr ls))))))

;; Exercise 2.18: remove-last
;; Define a procedure remove-last that removes the last top-level occurrence of a given element
;; item in a list ls.
;; (remove-last 'a '(b a n a n a s)) ==> (b a n a n s)
(define (remove-last item ls)
  (cond
   ((null? ls) '())
   ((not (member? item (cdr ls))) (cdr ls))
   (else (cons (car ls) (remove-last item (cdr ls))))))

;; Exercise 2.19: sandwich-1st
;; Define a procedure sandwich-1st that takes two items, a and b, and a list ls as its arguments.
;; It replaces the first occurence of two successive b's in ls with b a b.
;; (sandwich-1st 'meat 'bread '(bread chesse bread bread)) ==> (bread cheese bread meat bread)
(define (sandwich-1st meat bread ls)
  (cond
   ((null? ls) '())
   ((and (equal? bread (car ls))
         (equal? bread (cadr ls))) (cons bread (cons meat (cdr ls))))
   (else (cons (car ls) (sandwich-1st meat bread (cdr ls))))))

;; Exercise 2.20: list-of-symbols?
(define (list-of-symbols-cond? lst)
  (cond
   ((null? lst) #t)
   ((symbol? (car lst)) (list-of-symbols-cond? (cdr lst)))
   (else #f)))

(define (list-of-symbols-if? lst)
  (if (null? (cdr lst))
      (symbol? (car lst))
      (list-of-symbols-if (cdr lst))))

(define (list-of-symbols-ano? lst)
  (or (null? lst)
      (and (symbol? (car lst))
           (list-of-symbols-ano? (cdr lst)))))

;; Exercise 2.21: all-same?
;; Define a procedure all-same? that takes a list ls as its argument and tests
;; whether all top-level elements of ls are the same.
;; (all-same? '(a a a a a a a))
(define (all-same? lst)
  (cond
   ((null? lst) #t)
   ((null? (cdr lst)) #t)
   (else (and (equal? (car lst) (cadr lst))
              (all-same? (cdr lst))))))
