;; Program 6.1 string-insert
(define (string-insert insrt strng n)
  (string-append
   (substring strng 0 n)
   insrt
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

;; Program 6.5 round-n-place
(define (round-n-places n dec-num)
  (let ((scale-factor (expt 10 n)))
    (/ (round (* dec-num scale-factor))
       scale-factor)))

;; Program 6.6 read-demo
(define (read-demo)
  (display "Enter data (enter done when finished): ")
  (let ((response (read)))
    (cond
     ((eq? response 'done) (display "Thank you. Good-bye."))
     (else
      (display "You entered: ")
      (write response)
      (newline)
      (read-demo)))))

;; Program 6.7 interactive-square-root
(define (interactive-square-root)
  (begin
    (write "Enter the number whose square root you want,")
    (newline)
    (write " or enter done to quit:")
    (newline))
  (let ((n (read)))
    (if (eq? n 'done)
        (begin
          (write "That's all, folks.")
          (newline))
        (begin
          (begin
            (write "The square root of " n " is " (square-root n))
            (newline))
          (newline)
          (interactive-square-root)))))

;; Program 6.9 tower-of-hanoi
(define (tower-of-hanoi n)
  (letrec ((move (lambda (n source destination helper)
                   (if (= n 1)
                       (list (list source destination))
                       (append
                        (move (sub1 n) source helper destination)
                        (cons
                         (list source destination)
                         (move (sub1 n) helper destination source)))))))
    (move n 'L 'R 'C)))

;; Program 6.10 display-tower-of-hanoi
(define display-tower-of-hanoi
  (let ((show-move (lambda (s d)
                     (display s)
                     (display " --> ")
                     (display d))))
    (lambda (n)
      (letrec
          ((move
            (lambda (n source destination helper)
              (if (= n 1)
                  (begin
                    (show-move source destination)
                    (newline))
                  (begin
                    (move (sub1 n) source helper destination)
                    (show-move source destination)
                    (display ", ")
                    (move (sub1 n) helper destination source))))))
        (move n 'L 'R 'C)))))


;; Program 6.12 legal?
(define (legal? try legal-pl)
  (letrec
      ((good?
        (lambda (new-pl up down)
          (cond
           ((null? new-pl) #t)
           (else (let ((next-pos (car new-pl)))
                   (and
                    (not (= next-pos try))
                    (not (= next-pos up))
                    (not (= next-pos down))
                    (good? (cdr new-pl)
                           (add1 up)
                           (sub1 down)))))))))
    (good? legal-pl (add1 try) (sub1 try))))

(define (solution? legal-pl)
  (= (length (legal-pl) 8)))

(define fresh-try 8)

;; Program 6.13 build-solution
(define (build-solution legal-pl)
  (cond
   ((solution? legal-pl) legal-pl)
   (else
    (forward fresh-try legal-pl))))

;; Program 6.14 forward
(define (forward try legal-pl)
  (cond
   ((zero? try) (backtrack legal-pl))
   ((legal? try legal-pl) (build-solution (cons try legal-pl)))
   (else (forward (sub1 try) legal-pl))))

;; Program 6.15 backtrack
(define (backtrack legal-pl)
  (cond
   ((null? legal-pl) '())
   (else (forward (sub1 (car legal-pl)) (cdr legal-pl)))))
