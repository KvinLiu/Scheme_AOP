;; Program 7.1 map
(define (map proc ls)
  (if (null? ls)
      '()
      (cons (proc (car ls)) (map proc (cdr ls)))))

;; Program 7.2 for-each
(define (for-each proc ls)
  (if (not (null? ls))
      (begin
        (proc (car ls))
        (for-each proc (cdr ls)))))
