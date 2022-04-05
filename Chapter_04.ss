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
