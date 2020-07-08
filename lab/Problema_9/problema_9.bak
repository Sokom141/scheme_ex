;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problema_9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Parte 1
(define latin "ABCDEFGHILMNOPQRSTVX")

(define traslate
  (lambda (alphabet)
    (lambda (key)
      (string-append (substring alphabet key) (substring alphabet 0 key))
      )
    )
  )

(define get_alphabet (traslate latin))

(define position
  (lambda (char alph)
    (if (string=? char (substring alph 0 1))
        0
        (+ 1 (position char (substring alph 1)) )
        )
    )
  )

(define match
  (lambda (letter alph)
    (string (string-ref alph (position letter latin)))
    )
  )

(define crypt
  (lambda (input key)
    (cond ((string=? input "") "")
          ((string=? (substring input 0 1) " ") (crypt (substring input 1) key))
          (else (string-append (match (substring input 0 1) (get_alphabet key)) (crypt (substring input 1) key)))
          )
    )
  )

; Parte 2


(define H
  (lambda (f g)
    (lambda (m n)
      (if (= n 0)
          (f m)
          (g m ((H f g) m (- n 1)))
          )
      )
    )
  )

(define s2
  (lambda (m n)
    (+ n 1)
    )
  )

(define add
  (H (lambda (i) i) s2)
  )

(define mul
  (H (lambda (z) 0) add)
  )

(define pow
  (H (lambda (u) 1) mul)
  )
