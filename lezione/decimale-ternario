;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname decimale-ternario) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
;   -60 --> "-+-+."
;    n = 3q+r
;   -n' = 3(-q')+(-r')   n',q',r' >= 0
;    n = 3q - 3 - 2 + 3 = 3(q-1) +1   | r = -2
;    n = 3q + 3 + 3 - 3 = 3(q+1) -1   | r = +2

(define btd-rep  ; val: stringa 
  (lambda (v)   ;   v: -1, 0, +1
    (cond ((= v -1) "-")
          ((= v +1) "+")
          (else     ".")
          )
    )
  )

(define btr-rep ; val: stringa di -/./+
  (lambda (n)   ;   n: intero
    (if (<= (abs n) 1)
        (btd-rep n)
        (let ( (q (quotient n 3)) (r (remainder n 3)))
          (cond ((= r -2)
                 (string-append (btr-rep (- q 1)) (btd-rep +1))) ; con resto -2
                ((= r +2)
                 (string-append (btr-rep (+ q 1)) (btd-rep -1))) ; con resto +2
                (else
                 (string-append (btr-rep q) (btd-rep r)))        ; con resto -1/0/+1
                )
          )
        )
    )
  )
