;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname problema_3) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define find-point   ; val: indeice in cui si trova il punto nella stringa
  (lambda (s)        ;   s: stringa non vuota
    (let ( (len-s (string-length s)) )
      (let ( (p (string-ref s (- len-s 1)) ) )
        (if (char=? p #\.)
            (- len-s 1)
            (cond ((> len-s 1) (find-point (substring s 0 (- len-s 1)) ))
                  (else "")
                  )
            )
        )
      )
    )
  )


(define symbol   ; val: stringa con segno o vuota a seconda dell'input
  (lambda (s)    ;   s: stringa non vuota
    (let ( (f (string-ref s 0)) )
      (cond ((char=? f #\+) "+")
            ((char=? f #\-) "-")
            (else "")
            )
      )
    )
  )
;------------------------------------------------------------------------------------------------;

; TODO:
(define n-intero
  (lambda (si)
    (let ( (l (string-ref si (- (string-length si) 1))) )
      (string-append "" (number->string
                         (+
                          (if (char=? l #\1)
                              (expt 2 0)
                              (+ 0 0)
                              )
                          )
                         ))
      )
    )
  )

; TODO:
(define bin-rep->number
  (lambda (sn)
    (display "todo")
    )
  )