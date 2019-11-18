;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 15-11-Liste) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; costante: null
; predicato: null?
; procedura: car (primo elemento)
; procedura: cdr (lista meno il primo elemento)
; procedura: cons (crea una lista con il primo elemento + una lista di partenza)
; () = '() = null
; '(1 2 3 4 5)

#|
> (list-ref '("a" "b") 1)
"b"
> (list-ref '("a" "b") 0)
"a"
> (car '("a" "b"))
"a"
> (length '(1 2 43))
3
> (append  '(5) '(1 2 43))
(list 5 1 2 43)
> (reverse '(1 2 3))
(list 3 2 1)
|#

(define my-list-ref
  (lambda (l i)
    (if (= i 0)
        (car l)
        (my-list-ref (cdr l) (- i 1))
        )
    )
  )

(define my-append
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (append (cdr l1) l2))
        )
    )
  )

(define my-reverse
  (lambda (l)
    (if (null? l)
        null
        (append (my-reverse (cdr l)) (list (car l)))
        )
    )
  )

(define reverse-rec
  (lambda (l r)
    (if (null? l)
        r
        (reverse-rec (cdr l) (cons (car l) r))
        )
    )
  )

(define sorted-append
  (lambda (l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          ((< (car l1) (car l2))
           (cons (car l1) (sorted-append (cdr l1) l2)))
          (else
           (cons (car l2) (sorted-append l1 (cdr l2))))
          )
    )
  )