;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname problema_1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Autore: Mattia Pizzolitto
; Data:   21/10/2019

(define nothing         ; val: stringa
  (lambda (s)           ; s:   stringa
    (substring s 0)
    )
  )

(define check-last-chr  ; val: True o False
  (lambda (s chr)       ; s:   sostantivo   chr: #\carattere
    (char=? (string-ref s (- (string-length s) 1)) chr)
    )
  )

(define get-desinenza
  (lambda (verbo)
    (substring verbo (- (string-length verbo) 3))
    )
  )

(define add-art         ; val: sostantivo con articolo
  (lambda (s)           ; s:   sostantivo
    (cond
      ((check-last-chr s #\o)(string-append "il " s)) ; maschile singolare
      ((check-last-chr s #\i)(string-append "i "  s)) ; maschile plurale
      ((check-last-chr s #\a)(string-append "la " s)) ; femminile singolare
      ((check-last-chr s #\e)(string-append "le " s)) ; femminile plurale
      (else (nothing s))
      )
    )
  )

(define sm-plurale?
  (lambda (s)
      (check-last-chr s #\i)
    )
  )
(define sf-plurale?
  (lambda (s)
    (check-last-chr s #\e)
    )
  )

(define v-plurale?
  (lambda (v s des)
    (cond
      ((sm-plurale? s)(nothing des))
      ((sf-plurale? s)(nothing des))
      (else "")
      )
    )
  )

(define v-plurale-n?
  (lambda (s n)
    (cond
      ((sm-plurale? s)(+ n 0))
      ((sf-plurale? s)(+ n 0))
      (else (- n 1))
      )
    )
  )

(define componi
  (lambda (v s des n is_1)
    (string-append (substring v 0 (- (string-length v) (if (not is_1) (v-plurale-n? s n) (n)))) (v-plurale? v s des))
    )
  )

(define declina
  (lambda (verbo sogg)
    (cond
      ((string=? (get-desinenza verbo) "are")(componi verbo sogg "no" 2 #t))
      ((string=? (get-desinenza verbo) "ere")(componi verbo sogg "ono" 3 #f))
      ((string=? (get-desinenza verbo) "ire")(componi verbo sogg "ono" 3 #f))
      (else (nothing verbo))
      )
    )
 )

(define frase
  (lambda (sogg verbo ogg)
    (string-append (add-art sogg) " " (declina verbo sogg) " " (add-art ogg))
    )
  )


  (frase "gatto" "cacciare" "topi")
  (frase "mucca" "mangiare" "fieno")
  (frase "sorelle" "leggere" "novella")
  (frase "bambini" "amare" "favole")
  (frase "musicisti" "suonare" "pianoforti")
  (frase "cuoco" "friggere" "patate")
  (frase "camerieri" "servire" "clienti")
  (frase "mamma" "chiamare" "figlie")
