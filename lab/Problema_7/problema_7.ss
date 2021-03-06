;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problema_7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define belong? ; val: boolean
  (lambda (n l) ; n: intero, l: lista di interi
    (if (null? l)
        #f
        (if (= n (car l))
            #t
            (belong? n (cdr l)))
        )
    )
  )

(define position ; val: intero ( indice dell'elemento )
  (lambda (n l)  ; n: intero, l: lista di interi
    (if (belong? n l)
        (+ (if (= (car l) n)
               0
               (+ 1 (position n (cdr l)))
            ))
        "NONE"
        )
    )
  )

(define sorted-ins
  (lambda (n l)
    (if (null? l)
        (cons n l)
        (cond ((< n (car l)) (cons n l))
              ((= n (car l)) l)
              (else (cons (car l) (sorted-ins n (cdr l))))
            )
        )
    )
  )

(define sorted-list
  (lambda (l)
    (if (null? l)
        '()
        (sorted-ins (car l) (sorted-list (cdr l)))
        )
    )
  )


; Test:

(belong? 18 '()) ; →  false
(belong? 18 '(5 7 10 18 23)) ; →  true
(belong? 18 '(5 7 10 12 23)) ; →  false
(position 7 '(7 8 24 35 41)) ; →  0
(position 35 '(7 8 24 35 41)) ; →  3
(position 41 '(7 8 24 35 41)) ; →  4
(sorted-ins 24 '()) ; →  '(24)
(sorted-ins 5 '(7 8 24 35 41)) ;  →  '(5 7 8 24 35 41)
(sorted-ins 24 '(7 8 24 35 41)) ; →  '(7 8 24 35 41)
(sorted-ins 27 '(7 8 24 35 41)) ; →  '(7 8 24 27 35 41)
(sorted-list '(35 8 41 24 7)) ; →  '(7 8 24 35 41)