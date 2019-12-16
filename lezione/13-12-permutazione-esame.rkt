;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 13-12-permutazione-esame) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
(define permutation-rule
  (lambda (plain encrypted)
    (__ (permutation-rec ___ plain encrypted))
    )
  )


(define mirror
  (lambda (c)
    (permutation-rec c 0 "ABCDEFGHILMNOPQRSTVX"
                         "XVTSRQPONMLIHGFEDCBA")
    )
  )
|#

(define permutation-ruler
  (lambda (plain encrypted)
    (lambda (c)
      (permutation-rec c 0 plain encrypted)
      )
    )
  )

(define permutation-rec
  (lambda (x i plain encrypted)
    (if (char=? (string-ref plain i) x)
        (string-ref encrypted i)
        (permutation-rec x (+ i 1) plain encrypted)
        )
    )
  )

