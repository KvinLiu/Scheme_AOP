;; Program 6.1 string-insert
(define (string-insert insrt strng n)
  (string-append
   (substring strng 0 n)
   insert
   (substring strng n (string-length strng))))

;; Program 6.2 square-root
(define (square-root a)
  (letrec
      ((next-estimate
        (lambda (u)
          (let ((v (/ (+ u (/ a u)) 2)))
            (if (close-enough? u v)
                v
                (next-estimate v))))))
    (next-estimate 1)))

;; Program 6.3 square-root-display
(define (square-root-display a)
  (letrec ((next-estimate
            (lambda (u)
              (let ((v (/ (+ u (/ a u)) 2)))
                (if (close-enough? u v)
                    v
                    (begin
                      (display v)
                      (newline)
                      (next-estimate v)))))))
    (next-estimate 1)))
