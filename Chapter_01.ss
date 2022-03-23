;;; Exercise 1.6
;; a
(cons 'one (cons 'two (cons 'three (cons 'four '()))))
;; b
(cons 'one (cons (cons 'two (cons 'three (cons 'four '()))) '()))
;; c
(cons 'one (cons (cons 'two (cons 'three '())) (cons 'four '())))
;; d
(cons (cons 'one (cons 'two '())) (cons (cons 'three (cons 'four '())) '()))
;; e
(cons (cons (cons 'one '()) '()) '())

;; Exercise 1.7
;; n + 1

;; Exercise 1.8
'(a 'b)
;; quote list contains 'b

;; Exercise 1.9
;; If a and b evaluate to any values, what is

;; a
(car (cons a b))

;; b
(cdr (cons a b))

;; Exercise 1.10
;; a
(symbol? (cons a b)) ;; ==> #f

;; b
(pair? (cons a b)) ;; ==> #t

;; c
(null? (cons a b)) ;; ==> #f

;; d
(null? (cdr (cons a '()))) ;; ==> #t

;; Exercise 1.11
;; If a list ls contains only one item, what is (null? (cdr ls))?
;; #t

;; Exercise 1.12
;; a
(cdr '((a (b c) d)))  ;; => '()
;; b
(car (cdr (cdr '(a (b c) (d e)))))  ;; => e
;; c
(car (cdr '((1 2) (3 4) (5 6))))  ;; => (3 4)
;; d
(cdr (car '((1 2) (3 4) (5 6))))  ;; (2)
;; e
(cdr (cdr (car '((cat dog hen)))))  ;; => (hen)
;; f
(cadr '(a b c d))  ;; => b
;; g
(cadar '((a b) (c d) (e f)))  ;; ==> b

;; Exercise 1.13
;; a
(car (cdr (cdr '(b c a d))))  ;; => a
(caddr '(b c a d))  ;; ==> a
;; b
(car (cdr (car '((b a) (c d)))))  ;; ==> a
(cadar '((b a) (c d))) ;; ==> a
;; c
(car (car(cdr '((d c) (a) b))))   ;; ==> a
(caadr '((d c) (a) b))  ;; ==> a
;; d
(car (car (car '(((a))))))  ;; ==> a

;; Exercise 1.14
;; Decide whether the following expressions are true or false:
;; a
(symbol? (car '(cat mouse))) ;; #t
;; b
(symbol? (cdr '((cat mouse))))  ;; #f
;; c
(symbol? (cdr '(cat mouse))) ;; #f
;; d
(pair? (cons 'hound '(dog))) ;; #t
;; e
(pair? (car '(cheshire cat))) ;; #f
;; f
(pair? (cons '() '())) ;;t

;; Exercise 1.15
;; Decide whether the following expressions are true or false:
;; a
(eqv? (car '(a b)) (car (cdr '(b a)))) ;; #t
;; b
(eqv? 'flea (car (cdr '(dog flea)))) ;; #t
;; c
(eq? (cons 'a '(b c)) (cons 'a '(b c))) ;; #f
;; d
(eqv? (cons 'a '(b c)) (cons 'a '(b c))) ;; #f
;; e
(equal? (cons 'a '(b c)) (cons 'a '(b c))) ;; #t
;; f
(null? (cdr (cdr '((a b c) d)))) ;; #t
;; g
(null? (car '(()))) ;; #t
;; h
(null? (car '((())))) ;; #f
