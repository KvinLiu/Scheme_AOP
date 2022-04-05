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
