;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 29-11-mul-rec) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define mul     ; val: m n intero
  (lambda (m n) ; m,n: interi >= 0
    (mul-rec m n 0)
    )
  )

(define mul-rec   ;   val: mn+p intero
  (lambda (m n p) ; m,n,p: interi >=0
    (cond ((= n 0) p)
          ((even? n) (mul-rec (* 2 m) (quotient n 2) p ))
          (else      (mul-rec (* 2 m) (quotient n 2) (+ p m)))
          )
    )
  )

; Per ogni k € N, per ogni n € [0, k], per ogni m, p € N  (mul-rec m n p) --> mn+p

#|
Dimostrazione per induzione su k -> ampiezza intervallo
Casi base         k
 per ogni n € [0, 0]  xO n, p € N (mul-rec m n p) --> mn + p
==                    xO m, p     (mul-rec m 0 p) --> p

Ipotesi induttiva: Scelgo k € N e assumo:
 xO n € [0, k]  xO m,p € N (mul-rec m n p) --> mn+p

Passo induttivo: per le scelte sopra voglio dimostrare:
 xO n € [0, k+1] xO m,p € N (mul-rec m n p) --> mn+p
Mi basta verificare che vale anche:
 xO m, p € N (mul-rec m k+1 p) --> m(k+1)+p

Dimostrazione del passo induttivo: per k scelto a sinistra e xO m, p € N
(mul-rec m k+1 p) --> (cond ((= k+1 0)   ... ) ... )  k+1 non può essere == 0, poichè k è un numero naturale
		  --> (cond ((even? k+1) ... ) ... )
a) k dispari
	--> (mul-rec (* 2 m) (quotient k+1 2) p)
	--> (mul-rec 2m (k+1)/2 p) --> 2m ((k+1)/2) + p  per l'ipotesi induttiva

b) k pari -----> k+1 dispari
	--> (mul-rec (* 2 m) (quotient k+1 2) (+ p m))
	--> (mul-rec 2m k/2 p+m)
	--> 2m(k/2)+p+m		per l'ipotesi induttiva
	==  m(k+1)+p  OK
|#


(define ufo     ; val: intero    Unidentified Flying prOcedure
(lambda (x)
(cond ((= x 1) 1)
((even? x)
(- (* 2 (ufo (quotient x 2))) 1))
(else
(+ (* 2 (ufo (quotient x 2))) 1))
)))

(define manh  ;  val: intero
(lambda (i j) ; i, j: interi
(of (or (= i 0) (= j 0))
1
(+ (manh (- i 1) j) (manh i (- j 1)))
)))
