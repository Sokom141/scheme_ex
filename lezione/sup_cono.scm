;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sup_cono) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define apotema
  (lambda (r h)
    (sqrt (+ (quadrato r) (quadrato h)))
    )
  )


(define quadrato ; restituisce il quadrato della variabile inserita. val: reale
  (lambda (x) ; x: reale positivo
    (* x x)
    )
  )


(define sup-cono ; val: reale
  (lambda (r h) ; r, h: reali positivi
    (+ (* pi r (apotema r h)) (* 2 pi r))
    )
  )