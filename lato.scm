;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname lato) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define b0 (* 100 (expt 2 +1/4)))
(define b1 (* 100 (expt 2 -1/4)))

(define b        ; val: reali
    (lambda (k)  ; k: intero non negativo
      (if (< k 2)
          (if (= k 0) b0 b1)
          (/ (b (- k 2)) 2) 
       )
     )
  )
