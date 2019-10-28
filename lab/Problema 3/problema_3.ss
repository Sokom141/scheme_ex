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

(define parte-intera
  (lambda (s)
    (substring s 0 (find-point s))
    )
  )

(define parte-decimale
  (lambda (s)
    (substring s (+ (find-point s) 1))
    )
  )

; TODO:
(define n-intero
  (lambda (si)
    (let ( (len-s (string-length si)) )
      (let ( (fb (if (> len-s 0) (string->number (substring si 0 1)) 0)) ) ;fb: first bit
        (if (=  fb 1)
            (+ (expt 2 (- len-s 1)) (n-intero (substring si 1 len-s)))
            (if (= fb 0)
                (+  (if (> len-s 0)(n-intero (substring si 1 len-s ) ) 0))
                0
                )
            )
        )
      )
    )
  )

(define n-decimale
  (lambda 
  )

; TODO:
(define bin-rep->number
  (lambda (sn)
    (n-intero (parte-intera sn))
    )
  )