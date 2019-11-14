;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname problema_3_seconda_parte) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define find-point   ; val: indice in cui si trova il punto nella stringa
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

(define parte-intera   ; val: stringa parte intera
  (lambda (s)          ;   s: stringa non vuota
    (if (= (find-point s) -1)
        s
        (if (string=? s "1")
            "1"
            (substring s 0 (find-point s))
            ) ;nel caso in cui non ci sia il punto len+1
        )
    )
  )

(define parte-decimale  ; val: stringa parte decimale, vuota se la stringa in entrata non contiene parte decimale
  (lambda (s)           ;   s: stringa non vuota
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
(define n-intero   ; val: binario intero
  (lambda (si b)     ;  si: stringa non vuota
    (let ( (len-s (string-length si)) )
      (let ( (fb (if (> len-s 0) (string->number (substring si 0 1)) 0)) ) ;fb: first bit
        (if (= fb 1)
            (+ (expt b (- len-s 1)) (n-intero (substring si 1 len-s) b))
            (if (= fb 0)
                (+  (if (> len-s 0)(n-intero (substring si 1 len-s ) b) 0))
                0
                )
            )
        )
      )
    )
  )

(define n-decimale  ; val: binario decimale
  (lambda (sd b)      ;  sd: stringa 
    (if (string=? sd "")
        0
        (let ( (len-s (string-length sd)) )
          (let ( (lb (if (> len-s 0) (string->number (substring sd (- len-s 1))) 0)) )
            (if (= lb 1)
                (+ (expt b (- len-s)) (n-decimale (substring sd 0 (- len-s 1)) b))
                (if (= lb 0)
                    (+ (if (> len-s 0) (n-decimale (substring sd 0 (- len-s 1)) b) 0))
                    0
                    )
                )
            )
          )
        )
    )
  )

;---------------------------------------------------------------------------------------------;

(define bin-rep->number  ; val: numero in base binaria
  (lambda (sn)           ;  sn: stringa contenente almeno un numero
    (if (string=? sn "0")
        0
        (let ( (no-sym (substring sn 1) ) )
          (cond ((string=? (symbol sn) "+")
                 (+ (n-intero (parte-intera no-sym) 2) (n-decimale (parte-decimale sn) 2)))
                ((string=? (symbol sn) "-")
                 (- (n-intero (parte-intera no-sym) 2) (n-decimale (parte-decimale sn) 2)))
                (else (+ (n-intero (parte-intera sn) 2) (n-decimale (parte-decimale sn) 2)))
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


; seconda parte:

(define base
  (lambda (b)
    (string-length b)
    )
  )

(define n-intero-all   ; val: binario intero
  (lambda (si b)       ;  si: stringa non vuota
    (let ( (len-s (string-length si)) )
      (let ( (fb (if (> len-s 0) (string->number (substring si 0 1)) 0)) ) ;fb: first bit
        (if (> fb 0)
            (+ (expt b (- len-s 1)) (n-intero (substring si 1 len-s) b))
            (if (= fb 0)
                (+  (if (> len-s 0)(n-intero (substring si 1 len-s ) b) 0))
                0
                )
            )
        )
      )
    )
  )



(define rep->number
  (lambda (base num)
    (let ( (b (string-length base)) )
      (if (string=? num "0")
          0
          (let ( (no-sym (substring num 1) ) )
            (cond ((string=? (symbol num) "+")
                   (+ (n-intero-all (parte-intera no-sym) b) (n-decimale (parte-decimale num) b)))
                  ((string=? (symbol num) "-")
                   (- (n-intero-all (parte-intera no-sym) b) (n-decimale (parte-decimale num) b)))
                  (else
                   (+ (n-intero-all (parte-intera num b) (n-decimale (parte-decimale num b))))
                   )
            )
          )
      )
    )
  ))

;(rep->number "zu" "-uuzz")
(rep->number "0123" "+21.1")
(rep->number "01234" "-10.02") 