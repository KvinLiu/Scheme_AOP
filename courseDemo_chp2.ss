(define regroup
  (lambda (list-of-4)
    (make-list-of-two
     (first-group list-of-4)
     (second-group list-of-4))))

(define make-list-of-two
  (lambda (one two)
    (cons one (cons two '()))))

(define first-group
  (lambda (ls)
    (make-list-of-two
     (car ls)
     (cadr ls))))

(define second-group
  (lambda (ls)
    (cddr ls)))

(regroup '(1 2 3 4))
