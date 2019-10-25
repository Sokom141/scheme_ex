;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ricorsione) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Ricorsione -> possibile risolvere qualsiasi problema

(define cmp   ; val: stringa
  (lambda (b) ; b: stringa "0" oppure "1"
    (if (string=? b "0")
        "1"
        "0"
        )
    )
  )

(define compl-1 ; val: stringa
  (lambda (s)   ; s: stringa di 0/1
    (let ((k (quotient (string-length s)))
      )
#|    (if (> (string-length s) 0)
        (string-append
         (cmp (substring s 0 1))
         (compl-1 (substring s 1))
         )
        ""
        ) |#
    (if (> k 0)
        (string-append
         (compl-1 (substring s 0 k))
         (compl-1 (substring s k))
         )
        (cmp s)
        )
    )
  ))

(define bin-rep ; dal numero decimale restituisce binario
  (lambda (n)
    (let ( (q (quotient n 2)) (r (remainder n 2)) )
      (let ( (lsb (if (= r 0) "0" "1")) )
        (if (= q 0)
            lsb
            (string-append (bin-rep q) lsb)
            )))
    ))

#|
compito:
(define d0 (char->integer #\0))

(define rep
 (lambda (n B)
  (let ( (q (/ n B) (r (mod n B)))
    (let ( (lsd (string (int->chr (+ d0 r))) )
      (if (= q 0)
           lsd
           (string-append (rep q B) lsd)
            )))
))
(rep 21 5) -> 41
(rep 1024 2) -> "10000000000
(rep 507 3) -> 200210
|#



; (let ((k1 E1) (k2 E2) ..)

(define bin-val   ; val: intero
  (lambda (num)   ; num: stringa non vuota di 0/1
    (let ( (k (- (string-length num) 1)))
      (let ( (pre (substring num 0 k)) (lsb (string-ref num k )) )
        (if (= k 0)
            (bit-val lsb)
            (+ (* 2 (bin-val pre)) (bit-val lsb))
            )
        )
      )
    )
  )

(define bit-val
  (lambda (bit)
    (if (char=? bit #\0) 0 1)
    )
  )

; sistema ternario bilanciato
(define btr-val ; val: intero
  (lambda (num) ; num: stringa non vuota di -/./+
    (let (( k (-(string-length num) 1 ) ))
          (let ( (pre (substring num 0 k)) (lsb (string-ref num k)))
        (if (= k 0)
            (btd-val lsb)
            (+ (* 3 (btr-val pre)) (btd-val lsb))
            ))
      )))

(define btd-val
  (lambda (btd)
    (cond ((char=? btd #\-) -1)
          ((char=? btd #\.)  0)
          (else +1)
          )
    )
  )