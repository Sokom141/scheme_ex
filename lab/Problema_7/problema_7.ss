;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problema_7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define belong?
  (lambda (n l)
    (if (null? l)
        #f
        (if (= n (car l))
            #t
            (belong? n (cdr l)))
        )
    )
  )

(define position
  (lambda (n l)
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

(sorted-list '(6 4 2))