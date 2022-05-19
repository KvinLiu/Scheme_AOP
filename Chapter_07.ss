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

;; Program 7.3 add
(define add
  (letrec ((list-add
            (lambda (ls)
              (if (null? ls)
                  0
                  (+ (car ls) (list-add (cdr ls)))))))
    (lambda args
      (list-add args))))

;; Program 7.4 list
(define list (lambda args args))

;; Program 7.5 writln
(define (writeln args)
  (for-each display args)
  (newline))

;; Program 7.6 error
(define (error args)
  (display "Error: ")
  (for-each
   (lambda (value)
     (display " ")
     (display value))
   args)
  (newline)
  (reset))

;; Program 7.7 add
(define (add args)
  (if (null? args)
      0
      (+ (car args) (apply add (cdr args)))))
