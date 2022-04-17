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

;; Program 5.4: fact
(define (fact n)
  (letrec ((fact-it (lambda (k acc)
                      (if (zero? k)
                          acc
                          (fact-it (sub1 k) (* k acc))))))
    (fact-it n 1)))

;; Program 5.5: swapper
(define (swapper x y ls)
  (letrec
      ((swap (lambda (ls*)
               (cond
                ((null? ls*) '())
                ((equal? (car ls*) x) (cons y (swap (cdr ls*))))
                ((equal? (car ls*) y) (cons x (swap (cdr ls*))))
                (else (cons (car ls*) (swap (cdr ls*))))))))
    (swap ls)))

;; Program 5.6: zero-poly?
(define (zero-poly? poly)
  (and (zero? (degree poly))
       (zero? (leading-coef poly))))

;; Program 5.7: make-term
(define (make-term deg coef)
  (poly-cons deg coef the-zero-poly))

;; Program 5.8: leading-term
(define (leading-term poly)
  (make-term (degree poly) (leading-coef poly)))
