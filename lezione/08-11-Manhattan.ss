;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 08-11-Manhattan) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define manhattan
  (lambda (i j)
    (if (or (= i 0) (= j 0))
        1
        (+ (manhattan (- i 1) j) (manhattan i (- j 1)))
        )
    )
  )