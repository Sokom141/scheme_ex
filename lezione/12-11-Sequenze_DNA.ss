;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 12-11-Sequenze_DNA) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
; LCS: Longest Common Subsequence
; llcs (u, v) = ?

(define llcs     ;  val: intero
  (lambda (u v)  ; u, v: stringhe
    (cond ((or (string=? u "") (string=? v "")) 0)
          ((char=? (string-ref u 0) (string-ref v 0)) (+ 1 (llcs (substring u 1) (substring v 1))))
          (else
           (max (llcs (substring u 1) v) (llcs u (substring v 1))))
          )
    )
  )

; (llcs "AGACTGAACATAC" "GATCCGACTAC")

(define better
  (lambda (u v)
    (let ((m (string-length u)) (n (string-length v)))
      (cond ((< m n)
             v)
            ((> m n)
             u)
            (else (if (= (random 2) 0) u v))
            ))
    )
  )



(define lcs
  (lambda (u v)
    (cond ((or (string=? u "") (string=? v "")) "")
          ((char=? (string-ref u 0) (string-ref v 0)) (string-append (substring u 0 1) (lcs (substring u 1) (substring v 1))))
          (else
           (better (lcs (substring u 1) v)
                   (lcs u (substring v 1))
                   )
           )
          )
    )
  )