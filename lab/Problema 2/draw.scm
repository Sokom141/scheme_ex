;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname draw) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(set-puzzle-shift-step!)
(define s smaller-tile)
(define l larger-tile)

(define croce
  (lambda (st lt)
    (let ((1t (shift-right st 2)))
      (let ((bt (glue-tiles lt (shift-down (shift-right (half-turn lt) 2) 1))))
          (let ((2t (shift-down (shift-right (half-turn st) 2) 5) ))
            (glue-tiles (glue-tiles 1t bt) 2t)
            )
        )
      )
    )
  )

(define quadrato
  (lambda (st lt)
    (let ((1t (half-turn lt) ))
      (let ((bt (shift-down (shift-right lt 2) 1) ))
        (glue-tiles (glue-tiles (glue-tiles bt 1t) (shift-right (half-turn st) 2)) (shift-down (shift-right st 2) 5))
        ) 
      )
    )
  )


(croce s l)
(quadrato s l)
