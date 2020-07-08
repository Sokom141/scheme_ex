;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problema_9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Parte 1
(define latin "ABCDEFGHILMNOPQRSTVX") ; costante alfabeto latino

(define traslate  ; val: alfabeto traslato in base alla chiave
  (lambda (alphabet)
    (lambda (key)
      (string-append (substring alphabet key) (substring alphabet 0 key))
      )
    )
  )

(define get_alphabet (traslate latin))

(define position  ; val: indice carattere
  (lambda (char alph) ; char, alph: stringa
    (if (string=? char (substring alph 0 1))
        0
        (+ 1 (position char (substring alph 1)) )
        )
    )
  )

(define match  ; val: stringa carattere traslato
  (lambda (letter alph) ;letter,alph: stringhe
    (string (string-ref alph (position letter latin)))
    )
  )

(define crypt  ; val: stringa criptata
  (lambda (input key) ; input: stringa da criptare, key: chiave di criptazione
    (cond ((string=? input "") "")
          ((string=? (substring input 0 1) " ") (crypt (substring input 1) key))
          (else (string-append (match (substring input 0 1) (get_alphabet key)) (crypt (substring input 1) key)))
          )
    )
  )

; Test:

(crypt "ALEA IACTA EST IVLIVS CAESAR DIXIT" 3) ; -> DOHD NDFAD HXA NBONBX FDHXDV GNCNA

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

; Test:

(add 5 10) ; -> 15
(mul 9 9)  ; -> 81
(pow 3 3)  ; -> 27
