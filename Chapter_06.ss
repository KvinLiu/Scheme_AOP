;; Program 6.1 string-insert
(define (string-insert insrt strng n)
  (string-append
   (substring strng 0 n)
   insert
   (substring strng n (string-length strng))))
