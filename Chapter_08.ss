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

;; Program 8.14 intersection
(define (intersection s1 s2)
  (letrec
      ((helper
        (lambda (s1)
          (if (empty-set? s1)
              the-empty-set
              (let ((elem (pick s1)))
                (if ((contains s2) elem)
                    (adjoin elem (helper ((residue elem) s1)))
                    (helper ((residue elem) s1))))))))
    (helper s1)))

;; Program 8.15 union
(define (union s1 s2)
  (letrec
      ((helper
        (lambda (s1)
          (if (empty-set? s1)
              s2
              (let ((elem (pick s1)))
                (if (not ((contains s2) elm))
                    (adjoin elem (helper ((residue elem) s1)))
                    (helper ((residue elem) s1))))))))
    (helper s1)))

;; Program 8.16 difference
(define (difference s1 s2)
  (letrec
      ((helper
        (lambda (s1)
          (if (empty-set? s1)
              the-empty-set
              (let ((elem (pick s1)))
                (if (not ((contains s2) elem))
                    (adjoin elem (helper ((residue elem) s1)))
                    (helper ((residue elem) s1))))))))
    (helper s1)))

;; Program 8.17 set-builder
(define (set-builder pred base-set)
  (letrec
      ((helper
        (lambda (s)
          (if (empty-set? s)
              base-set
              (let ((elem (pick s)))
                (if (pred elem)
                    (adjoin elem (helper ((residue elem) s)))
                    (helper ((residue elem) s))))))))
    helper))

;; Program 8.19 family-union
(define (family-union s)
  (if (empty-set? s)
      the-empty-set
      (let ((elem (pick s)))
        (union elem (family-union ((residue elem)))))))

;; Program 8.20 family-intersection
(define (family-intersection s)
  (if (empty-set? s)
      the-empty-set
      (letrec
          ((fam-int
            (lambda (s)
              (let ((elem (pick s)))
                (let ((rest ((residue elem) s)))
                  (if (empty-set? rest)
                      elem
                      (intersection elem (fam-int rest))))))))
        (fam-int s))))

;; Program 8.21 set-map
(define (set-map proc s)
  (if (empty-set? s)
      the-empty-set
      (let ((elem (pick s)))
        (adjoin (proc elem)
                (set-map proc ((residue elem) s))))))

;; Program 8.22 list -> set
(define (list->set ls)
  (apply make-set ls))

;; Program 8.23 set -> list
(define (set->list s)
  (if (empty-set? s)
      '()
      (let ((elem (pick s)))
        (cons elem (set->list ((residue elem) s))))))
