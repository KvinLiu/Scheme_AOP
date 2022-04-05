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
