;; Program 4.1: append
(define (append ls1 ls2)
  (if (null? ls1)
      ls2
      (cons (car ls1) (append (cdr ls1) ls2))))

;; Program 4.2: reverse
(define (reverse ls)
  (if (null? ls)
      '()
      (append (reverse (cdr ls)) (list (car ls)))))

(define (reverse-custom ls temp)
  (cond
   ((null? ls) temp)
   (else (reverse (cdr ls) (cons (car ls) temp)))))

;; Program 4.3: merge
(define (merge stpl1 stpl2)
  (cond
   ((null? stpl1) stpl2)
   ((null? stpl2) stpl1)
   ((< (car stpl1) (car stpl2))
    (cons (car stpl1) (merge (cdr stpl1) stpl2)))
   (else (cons (car stpl2) (merge stpl1 (cdr stpl2))))))

;; Program 4.4: even?
(define (even-custom? n)
  (if (zero? n)
      #t
      (odd-custom? (sub1 n))))

;; Program 4.5: odd?
(define (odd-custom? n)
  (if (zero? n)
      #f
      (even-custom? (sub1 n))))

;; Program 4.6: remove
(define (remove-c item lst)
  (cond
   ((null? lst) '())
   ((equal? item (car lst)) (remove-c item (cdr lst)))
   (else (cons (car lst) (remove-c item (cdr lst))))))

;; Program: 4.7: count-all
(define (count-all ls)
  (cond
   ((null? ls) 0)
   ((not (pair? (car ls))) (add1 (count-all (cdr ls))))
   (else (+ (count-all (car ls)) (count-all (cdr ls))))))

(define (count-all-two ls)
  (cond
   ((null? ls) 0)
   (else (+ (if (pair? (car ls))
                (count-all-two (car ls))
                1)
            (count-all-two (cdr ls))))))

(define (count-all-three ls)
  (cond
   ((null? ls) 0)
   ((pair? (car ls))
    (+ (count-all-three (car ls)) (count-all-three (cdr ls))))
   (else (+ 1 (count-all-three (cdr ls))))))

;; Program 4.8: remove-all
(define (remove-all item ls)
  (cond
   ((null? ls) '())
   ((equal? item (car ls)) (remove-all item (cdr ls)))
   ((pair? (car ls))
    (cons (remove-all item (car ls)) (remove-all item (cdr ls))))
   (else (cons (car ls) (remove-all item (cdr ls))))))

(define (remove-all-2 item ls)
  (cond
   ((null? ls) '())
   ((equal? (car ls) item) (remove-all item (cdr ls)))
   (else (cons (if (pair? (car ls))
                   (remove-all-2 item (car ls))
                   (car ls))
               (remove-all-2 item (cdr ls))))))

;; Program 4.9: remq-all
(define (remq-all symbl ls)
  (cond
   ((null? ls) '())
   ((eq? symbl (car ls)) (remq-all symbl (cdr ls)))
   (else (cons (if (pair? (car ls))
                   (remq-all symbl (car ls))
                   (car ls))
               (remq-all symbl (cdr ls))))))

(define (remq-all-2 symbl ls)
  (cond
   ((null? ls) '())
   ((pair? (car ls))
    (cons (remq-all-2 symbl (car ls)) (remq-all-2 symbl (cdr ls))))
   ((eq? symbl (car ls)) (remq-all-2 symbl (cdr ls)))
   (else
    (cons (car ls) (remq-all-2 symbl (cdr ls))))))

;; Program 4.10: reverse-all
(define (reverse-all ls)
  (cond
   ((null? ls) '())
   ((pair? (car ls))
    (append (reverse-all (cdr ls)) (list (reverse-all (car ls)))))
   (else (append (reverse-all (cdr ls)) (list (car ls))))))

(define (reverse-all-2 ls)
  (if (null? ls)
      '()
      (append (reverse-all-2 (cdr ls))
              (list (if (pair? (car ls))
                        (reverse-all-2 (car ls))
                        (car ls))))))

;; Program 4.13: depth
(define (depth item)
  (if (not (pair? item ))
      0
      (max (add1 (depth (car item))) (depth (cdr item)))))

;; Program 4.14: flatten
(define (flatten ls)
  (cond
   ((null? ls) '())
   ((pair? (car ls)) (append (flatten (car ls)) (flatten (cdr ls))))
   (else
    (cons (car ls) (flatten (cdr ls))))))

;; Program 4.15: remove-leftmost
(define (remove-leftmost item ls)
  (cond
   ((null? ls) '())
   ((equal? item (car ls)) (cdr ls))
   ((not (pair? (car ls)))
    (cons (car ls) (remove-leftmost item (cdr ls))))
   ((member-all? item (car ls))
    (cons (remove-leftmost item (car ls)) (cdr ls)))
   (else (cons (car ls) (remove-leftmost item (cdr ls))))))

;; Program 4.16: member-all?
(define (member-all? item ls)
  (if (null? ls)
      #f
      (or (equal? (car ls) item)
          (and (not (pair? (car ls)))
               (member-all? item (cdr ls)))
          (and (pair? (car ls))
               (or (member-all? item (car ls))
                   (member-all? item (cdr ls)))))))

;; Program 4.17: remove-leftmost
(define (remove-leftmost item ls)
  (cond
   ((null? ls) '())
   ((equal? (car ls) item) (cdr ls))
   ((and (pair? (car ls)) (member-all? item (car ls)))
    (cons (remove-leftmost item (car ls)) (cdr ls)))
   (else (cons (car ls) (remove-leftmost item (cdr ls))))))

;; Program 4.18: fact
(define (fact n)
  (if (zero? n)
      1
      (* n (fact (sub1 n)))))

;; Program 4.19: fact-it
(define (fact-it n acc)
  (if (zero? n)
      acc
      (fact-it (sub1 n) (* acc n))))

;; Program 4.20: fib
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;; Program 4.24 fib-it
(define (fib-it n acc1 acc2)
  (if (= n 1)
      acc2
      (fib-it (sub1 n) acc2 (+ acc1 acc2))))

;; Program 4.25 reverse-it
(define (reverse-it ls acc)
  (if (null? ls)
      acc
      (reverse-it (cdr ls) (cons (car ls) acc))))
