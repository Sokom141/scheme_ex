;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 22-11-mul) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define mul      ; val: intero
  (lambda (m n)  ; m,n: interi non negativi
    (cond ((= n 0) 0)
          ((even? n) (mul (* 2 m) (quotient n 2)))
          (else (+ m (mul (* 2 m) (quotient n 2)))))
    )
  )

#|
5 25
+ 5 (mul 10 12)
mul 20 6
mul 40 3
+ 40 (mul 80 1)
+80 (mul 160 0)
0
|#

(define my-gcd   ; val: intero
  (lambda (m n)  ; m,n: interi positivi
    (cond ((= m n) m)
          ((< m n) (my-gcd m (- n m)))
          (else (my-gcd (- m n) n))
          )
    )
  )

(define mul-rec
  (lambda (m n p)
    (cond ((= n 0) p)
          ((even? n) (mul-rec (* 2 n) (quotient n 2) p))
          (else (mul-rec (* 2 n) (quotient n 2) (+ p m)))
          )
    )
  )

(define power
  (lambda (m n)
    (power-rec m n 1)
    )
  )

(define power-rec
  (lambda (m n p)
    (cond ((= n 0) p)
          (else (+ 0 0) ; TODO)
          )
    )
  ))