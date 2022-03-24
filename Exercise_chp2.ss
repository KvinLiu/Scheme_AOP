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
