;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 05-11-Rapp_Bin_Successiva) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
; Rappresentazione Binaria Successiva

(define add-bin
  (lambda (str)
    (let ((lsb (substring str (- (string-length str) 1))) (delegate (substring str 0 (- (string-length str) 1))))
      (cond ((string=? lsb "0") (string-append delegate "1"))
            ((string=? lsb "1") (string-append (add-bin delegate) "0"))
            )
      )
    )
  )

(define check-one
  (lambda (str)
    (let ((lsb (substring str (- (string-length str) 1))))
      (if (string=? lsb "0")
          (add-bin str)
          (add-bin (string-append "0" str))
          )
      )
    )
  )

(define normalize
  (lambda (str)
    (let (( fb (substring str 0 1) ))
      (if (string=? fb "0")
          (normalize (substring str 1))
          str)
      )
    )
  )

(define bin-succ
  (lambda (str)
    (if (< (string-length str) 1)
        0
        (normalize (check-one str))
        )
    )
  )