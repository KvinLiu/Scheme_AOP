;; Program 5.3: remove-leftmost
(define (remove-leftmost item ls)
  (cond
   ((null? ls) '())
   ((equal? (car ls) item) (cdr ls))
   ((pair? (car ls))
    (let ((rem-list (remove-leftmost item (car ls))))
      (cons rem-list (cond
                      ((equal? (car ls) rem-list)
                       (remove-leftmost item (cdr ls)))
                      (else (cdr ls))))))
   (else (cons (car ls) (remove-leftmost item (cdr ls))))))
