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
