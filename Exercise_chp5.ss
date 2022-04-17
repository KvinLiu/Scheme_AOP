;; Exercise 5.1
;; Find the value of each of the following expressions, writing the local
;; environments for each of the nested let expressions. Draw arrows from
;; each variable to the parameter to which it is bound in a lambda or let
;; expression. Also draw an arrow from the parameter to the value to which
;; it is bound.
;; a.
(let ((a 5))                     ;;
  (let ((fun (lambda (x) (max x a)))) ;; max --> global, a --> upper a 5
    (let ((a 10)
          (r 20))
      (fun 1))))                 ;; fun -> lambda (x) (max x a), res --> 5

;; b.
(let ((a 1) (b 2))
  (let ((b 3) (c (+ a b)))       ;; a --> upper a 1, b --> upper b 2
    (let ((b 5))
      (cons a (cons b (cons c '())))))) ;; a -> 1, b -> 5, c -> 3,
