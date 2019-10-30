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
                  (else -1) ;(+ len-s 1))
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
    (if (= (find-point s) -1)
        s
        (if (string=? s "1")
            "1"
            (substring s 0 (find-point s))
            ) ;nel caso in cui non ci sia il punto len+1
        )
    )
  )

(define parte-decimale
  (lambda (s)
    (if (< (find-point s) (string-length s) )
        (if (= (find-point s) -1)
            ""
            (substring s (+ (find-point s) 1)))
        ""
        )
    )
  )

;-----------------------------------------------------------------------------------------------;

; TODO:
(define n-intero
  (lambda (si)
    (let ( (len-s (string-length si)) )
      (let ( (fb (if (> len-s 0) (string->number (substring si 0 1)) 0)) ) ;fb: first bit
        (if (= fb 1)
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
  (lambda (sd)
    (if (string=? sd "")
        0
        (let ( (len-s (string-length sd)) )
          (let ( (lb (if (> len-s 0) (string->number (substring sd (- len-s 1))) 0)) )
            (if (= lb 1)
                (+ (expt 2 (- len-s)) (n-decimale (substring sd 0 (- len-s 1))))
                (if (= lb 0)
                    (+ (if (> len-s 0) (n-decimale (substring sd 0 (- len-s 1))) 0))
                    0
                    )
                )
            )
          )
        )
    )
  )

;---------------------------------------------------------------------------------------------;

(define bin-rep->number
  (lambda (sn)
    (if (string=? sn "0")
        0
        (let ( (no-sym (substring sn 1) ) )
          (cond ((string=? (symbol sn) "+")
                 (+ (n-intero (parte-intera no-sym)) (n-decimale (parte-decimale sn) )))
                ((string=? (symbol sn) "-")
                 (- (n-intero (parte-intera no-sym)) (n-decimale (parte-decimale sn))))
                (else (+ (n-intero (parte-intera sn)) (n-decimale (parte-decimale sn))))
                )
          )
        )
    )
  )

#|
(if (string=? sn "0")
                      0
                      (+ (n-intero (parte-intera sn)) (n-decimale (parte-decimale sn)))
                      ) )
(+ (n-intero (parte-intera sn)) (n-decimale (parte-decimale sn)))

|#

; Test:  
(bin-rep->number "+1101")           ; -> 13
(bin-rep->number "0")               ; -> 0
(bin-rep->number "10110.011")       ; -> 22.375
(bin-rep->number "-0.1101001")      ; -> -0.8203125