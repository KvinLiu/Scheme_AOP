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
