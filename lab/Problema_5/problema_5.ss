;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problema_5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define manhattan
  (lambda (i j)
    (if (or (= i 0) (= j 0))
        1
        (+ (manhattan (- i 1) j) (manhattan i (- j 1)))
        )
    )
  )

(define manhattan-3d  ; val: numero di percorsi diversi di lunghezza minima
  (lambda (i j k)     ; interi non negativi. i: numero di piani, j: numero di stanze est-ovest, k: numero distanze nord-sud
    (if (or (and (= i 0) (= j 0) (= k 0)) (and (= i 0) (= j 0)) (and (= i 0) (= k 0)) (and (= j 0) (= k 0)))
        1
        (cond ( (= i 0) (+ (manhattan (- j 1) k) (manhattan j (- k 1))) )
              ( (= j 0) (+ (manhattan (- i 1) k) (manhattan i (- k 1))) )
              ( (= k 0) (+ (manhattan (- i 1) j) (manhattan i (- j 1))) )
              (else (+ (manhattan-3d (- i 1) j k) (manhattan-3d i (- j 1) k) (manhattan-3d i j (- k 1))))
              )
        )
    )
  )