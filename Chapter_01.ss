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

;; 1.7
;; n + 1

;; 1.8
'(a 'b)
;; quote list contains 'b
