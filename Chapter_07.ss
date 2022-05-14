;; Program 7.1 map
(define (map proc ls)
  (if (null? ls)
      '()
      (cons (proc (car ls)) (map proc (cdr ls)))))
