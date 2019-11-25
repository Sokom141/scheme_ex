;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problema_8) (read-case-sensitive #t) (teachpacks ((lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hanoi.ss" "installed-teachpacks")) #f)))
(define hanoi-moves ; val: lista di coppie
  (lambda (n) ; n > 0 intero
    (hanoi-rec n 1 2 3)
    )
  )

(define hanoi-rec ; val: lista di coppie
  (lambda (n s d t) ; n intero, s, d, t: posizioni
    (if (= n 1)
        (list (list s d))
        (let ((m1 (hanoi-rec (- n 1) s t d))
              (m2 (hanoi-rec (- n 1) t d s))
              )
          (append m1 (cons (list s d) m2))
          )
        )
    )
  )

