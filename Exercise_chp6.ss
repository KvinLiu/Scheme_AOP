;; Exercise 6.1: substring?
;; Define a predicate substring? with two parameters, sstr and strng, that
;; tests whether the string sstr is a substring of strng. Hint: This can be
;; done using string-length, substring, and string=?. Test your predicate on
;; the following:
;; (substring? "s a s" "This is a string.") => #t
;; (substring? "ringer" "This is a string.") => #f
;; (substring? "" This is a string.") => #t
(define (substring? sstr strng)
  (let ((sstrLen (string-length sstr))
        (strngLen (string-length strng)))
    (cond
     ((> sstrLen strngLen) #f)
     ((string=? sstr (substring strng 0 sstrLen)) #t)
     (else
      (substring? sstr (substring strng 1 strngLen))))))
