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

;; Program 8.7 for-all
(define (for-all pred)
  (none (compose not pred)))

;; Program 8.8 set-equal
(define (set-equal obj1)
  (lambda (obj2)
    (or (and ((neither set?) obj1 obj2)
             (equal? obj1 obj2))
        (and ((both set?) obj1 obj2)
             ((subset obj1) obj2)
             ((subset obj2) obj1)))))

;; Program 8.9 element
(define element
  (compose there-exists set-equal))

;; Program 8.10 contains
(define (contains set)
  (lambda (obj)
    ((element obj) set)))

;; Program 8.11 superset
(define (superset s1)
  (lambda (s2)
    ((for-all (contains s1)) s2)))

;; Program 8.12 subset
(define (subset s1)
  (lambda (s2)
    ((superset s2) s1)))

;; Program 8.13 cardinal
(define (cardinal s)
  (if (empty-set? s)
      0
      (let ((elem (pick s)))
        (add1 (cardinal ((residue elem) s))))))
