;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define shared
  (lambda (u v)
    (cond ((or (null? u) (null? v))
           null)
          ((= (car u) (car v))
           (cons (car u) (shared (cdr u) (cdr v))))
          ((> (car u) (car v))
           (shared u (cdr v)))
          (else
           (shared (cdr u) v)))
    )
  )