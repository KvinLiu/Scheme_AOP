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
