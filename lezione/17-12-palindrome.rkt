;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 17-12-palindrome) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define palindrome? 
  (lambda (s)
    (let ((n (string-length s)))
      (if (or (= n 0) (= n 1))
          #t
          (if (char=? (string-ref s 0) (string-ref s (- n 1)))
              (palindrome? (substring s 1 (- n 1)))
              #f
              )
          )
      )
    )
  )

(define palindrome-lev   ; val: intero
  (lambda (s)            ;   s: stringa
    (let ((n (string-length s)))
      (if (or (= n 0) (= n 1))
          n
          (if (char=? (string-ref s 0) (string-ref s (- n 1)))
              (+ 1 (palindrome-lev (substring s 1 (- n 1))))
              (palindrome-lev (substring s 1 (- n 1)))
              )
          )
      )
    )
  )

; ----------------------------------------------------------------------- ;

(define xlcs     ; val: stringa
  (lambda (s t)  ; s,t: stringhe
    (cond ((string=? s "") t)
          ((string=? t "") (string-append "/" (xlcs (substring s 1) t)))
          ((char=? (string-ref s 0) (string-ref t 0)) (string-append "*" (xlcs (substring s 1) (substring t 1))))
          (else (better (string-append "/" (xlcs (substring s 1) t)) (string-append (substring t 0 1) (xlcs s (substring t 1)))))
          )
    )
  )

(define better
  (lambda (u v)
    (if (< (stars u) (stars v))
        v
        u
        )
    )
  )

(define stars
  (lambda (q)
    (if (string=? q "")
        0
        (let ((n (stars (substring q 1))))
          (if (char=? (string-ref q 0) #\*)
              (+ n 1)
              n)
          )
        )
    )
  )