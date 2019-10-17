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
    (if (> (string-length s) 0)
        (string-append
         (cmp (substring s 0 1))
         (delega (substring s 1))
         )
        ""
        )
    )
  )