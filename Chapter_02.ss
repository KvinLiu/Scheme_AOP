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

;; Pre Program 2.5
(define writeln
  (lambda args
    (for-each display args)
    (newline)))

;; Program 2.5 remove-1st-trace
(define (remove-1st-trace item ls)
  (cond
   ((entering (null? ls) ls 1)
    (leaving '() 1))
   ((entering (equal? (car ls) item) ls 2)
    (leaving (cdr ls) 2))
   ((entering 'else ls 3)
    (leaving
     (cons (car ls) (remove-1st-trace item (cdr ls))) 3))))

;; Program 2.6 entering
(define entering
  (lambda (test input cond-clause-number)
    (begin
      (if test
          (writeln "    Entering cond-clause-"
                       cond-clause-number
                       " with ls = "
                       input))
      test)))

;; Program 2.7 leaving
(define leaving
  (lambda (result cond-clause-number)
    (begin
      (writeln "Leaving cond-clause-"
              cond-clause-number
              " with result = "
              result)
      result)))

;; Program 2.8: swapper
(define (swapper x y lst)
  (cond
   ((null? lst) '())
   ((equal? x (car lst)) (cons y (swapper x y (cdr lst))))
   ((equal? y (car lst)) (cons x (swapper x y (cdr lst))))
   (else (cons (car lst) (swapper x y (cdr lst))))))

(define (swapper-n x y lst)
  (cond
   ((null? lst) '())
   (else (cons (swap-tester x y (car lst))
               (swapper-n x y (cdr lst))))))

(define (swap-tester x y item)
  (cond
   ((equal? item x) y)
   ((equal? item y) x)
   (else item)))
