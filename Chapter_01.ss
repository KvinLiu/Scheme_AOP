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
