;; Program 8.1 both
(define (both pred arg1 arg2)
  (and (pred arg1) (pred arg2)))

;; Program 8.2 neither
(define (neither pred)
  (lambda (arg1 arg2)
    (not (or (pred arg1) (pred arg2)))))

;; Program 8.3 at-least-one
(define (at-least-one pred)
  (lambda (arg1 arg2)
    (or (pred arg1) (pred arg2))))

;; Program 8.4 make-set
(define (make-set args)
  (letrec
      ((list-make-set
        (lambda (args-list)
          (if (null? args-list)
              the-empty-set
              (adjoin
               (car args-list)
               (list-make-set (cdr args-list)))))))
    (list-make-set args)))

;; Program 8.5 none
(define (none pred)
  (letrec
      ((test
        (lambda (s)
          (or (empty-set? s)
              (let ((elem (pick s)))
                (and (not (pred elem))
                     (test ((residue elem) s))))))))
    test))

;; Program 8.6 there-exists
(define (there-exists pred)
  (compose not (none pred)))
