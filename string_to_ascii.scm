;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname string_to_ascii) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define return-chr
  (lambda (stringa position)
    (string-ref stringa position)
    )
  )

(define return-ascii
  (lambda (name pos)
    (char->integer (return-chr name pos))
    )
  )
