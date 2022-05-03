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

;; Exercise 6.2: string-reverse
;; Define a procedure string-reverse that takes a string as its argument and returns
;; a string that is the given string with its characters in reverse order.
;; Hint: You may find the following procedure useful
(define substring-ref
  (lambda (strng n)
    (substring strng n (add1 n))))
;; Test your procedure on the following:
;; (string-reverse "Jack and Jill") => "lliJ dna kcaJ"
;; (string-reverse "mom n dad") => "dad n mom"
;; (string-reverse "") => ""
(define (string-reverse strng)
  (letrec ((strRhelp (lambda (str beg)
                       (if (zero? (string-length str))
                           beg
                           (strRhelp (substring str 1 (string-length str))
                                     (string-append (substring-ref str 0) beg))))))
    (strRhelp strng "")))

;; Exercise 6.3: palindrome?
;; A string is a palindrome if the reverse of the string is the same as the string.
;; For example, "mom" and "dad" are examples of palindromes. Define a predicate
;; palindrome? that tests whether a given string is a palindrome. Test your predicate
;; on the following:
;; (palindrome? "able was I ere I saw elba") => #t
;; (palindrome? "mom n dad") => #f
(define (palindrome? strng)
  (string=? strng (string-reverse strng)))
