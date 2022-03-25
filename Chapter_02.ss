;; Program 2.1
(define singleton-list?
  (lambda (ls)
    (and
     (pair? ls)
     (null? (cdr ls)))))

;; Program 2.2
(define last-item
  (lambda (ls)
    (cond
     ((null? (cdr ls)) (car ls))
     (else (last-item (cdr ls))))))

;; Program 2.3
(define member?
  (lambda (item ls)
    (cond
     ((null? ls) #t)
     (else (or (equal? item (car ls))
               (member? item (cdr ls)))))))

(define memq?
  (lambda (item ls)
    (cond
     ((null? ls) #f)
     (else (or (eq? item (car ls))
               (memq? item (cdr ls)))))))

(define (memv? item ls)
  (cond
   ((null? ls) #f)
   (else (or (eqv? item (car ls))
             (memv? item (cdr ls))))))

;; Program 2.4
(define remove-1st
  (lambda (item lst)
    (cond
     ((null? lst) '())
     ((equal? (car lst) item) (cdr lst))
     (else (cons (car lst) (remove-1st item (cdr lst)))))))

(define (remq-1st item lst)
  (cond
   ((null? lst) '())
   ((eq? item (car lst)) (cdr lst))
   (else (cons (car lst) (remq-1st item (cdr lst))))))

(define (remv-1st item lst)
  (cond
   ((null? lst) '())
   ((eqv? item (car lst)) (cdr lst))
   (else (cons (car lst) (remv-1st item (cdr lst)))))
