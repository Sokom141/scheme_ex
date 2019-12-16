;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define num-sum
  (lambda (ns)
    (cond ((string=? (substring ns 1) "") (string->number ns))
          ((string=? (substring ns 0 1) "1") (+ 1 (num-sum (substring ns 1))))
          (else (+ 0 (num-sum (substring ns 1))))
    )
  )
  )


(define parity-check-failures
  (lambda (lsb)
    (parity-check-failures-rec lsb 0)
    )
  )

(define parity-check-failures-rec
  (lambda (lsb c)
    (cond ((null? lsb) null)
          ((even? (num-sum (car lsb))) (parity-check-failures-rec (cdr lsb) (+ c 1)))
          (else (cons c (parity-check-failures-rec (cdr lsb) (+ c 1))))
      )
    )
  )
 