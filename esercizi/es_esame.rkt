;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_esame) (read-case-sensitive #t) (teachpacks ((lib "hanoi.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hanoi.rkt" "installed-teachpacks")) #f)))
(define count  ; val: numero di occorrenze di n in l
  (lambda (l n) ; l: lista, n: intero
    (count-rec l n 0)
    )
  )

(define count-rec
  (lambda (l n c)
    (cond ((= (length l) 0) c)
          ((= (car l) n) (count-rec (cdr l) n (+ c 1)))
          (else (count-rec (cdr l) n c))
        )
    )
  )
