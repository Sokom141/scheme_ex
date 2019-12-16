;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problema_9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Parte 1
(define latin "ABCDEFGHILMNOPQRSTVX")

(define traslate
  (lambda (alphabet)
    (lambda (n)
      (string-append (substring alphabet n) (substring alphabet 0 n))
      )
    )
  )

(define get_alphabet (traslate latin))

(define position
  (lambda (l alph)
    (if (string=? l (substring alph 0 1))
        0
        (+ 1 (position l (substring alph 1)) )
        )
    )
  )

(define match
  (lambda (letter alph)
    (string (string-ref alph (position letter latin)))
    )
  )

(define crypt
  (lambda (in n)
    (cond ((string=? in "") "")
          ((string=? (substring in 0 1) " ") (crypt (substring in 1) n))
          (else (string-append (match (substring in 0 1) (get_alphabet n)) (crypt (substring in 1) n)))
          )
    )
  )


; Parte 2
