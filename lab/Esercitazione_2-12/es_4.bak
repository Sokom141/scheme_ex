;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define cyclic-string
  (lambda (pattern length)
    (let ((len (string-length pattern)))
      (if (> len length)
          (substring pattern 0 length)
          (string-append pattern (cyclic-string pattern (- length len)))
          )
      )
    )
  )
