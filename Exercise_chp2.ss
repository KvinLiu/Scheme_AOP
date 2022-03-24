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
