;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname problema_1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Autore: Mattia Pizzolitto
; Data:   21/10/2019

; stringa vuota
(define nothing         ; val: stringa
  (lambda (s)           ; s:   stringa
    (substring s 0)
    )
  )

; confronta gli ultimi due caratteri per sapere se sono uguali
(define check-last-chr  ; val: True o False
  (lambda (s chr)       ; s:   sostantivo   chr: #\carattere
    (char=? (string-ref s (- (string-length s) 1)) chr)
    )
  )

; restituisce la stringa di tre caratteri che compone la desinenza
(define get-desinenza   ; val:   stringa
  (lambda (verbo)       ; verbo: verbo all'infinito
    (substring verbo (- (string-length verbo) 3))
    )
  )

; Giustappone il carattere corretto in base alla condizione
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

; Il sostantivo è maschile plurale?
(define sm-plurale?     ; val: True o False
  (lambda (s)           ; s:   stringa
      (check-last-chr s #\i)
    )
  )

; Il sostantivo è femminile plurale?
(define sf-plurale?     ; val: True o False
  (lambda (s)           ; s:   stringa
    (check-last-chr s #\e)
    )
  )

; Il verbo è al plurale?
(define v-plurale?      ; val: 
  (lambda (v s des)     ; v, s, des: 
    (cond
      ((sm-plurale? s)(nothing des))
      ((sf-plurale? s)(nothing des))
      (else "")
      )
    )
  )

; todo
(define v-plurale-n?   ;  val:
  (lambda (s n)        ; s, n:
    (cond
      ((sm-plurale? s)(+ n 0))
      ((sf-plurale? s)(+ n 0))
      (else (- n 1))
      )
    )
  )

; compone la stringa 
(define componi              ;                val:
  (lambda (v s des n is_1)   ; v, s, des, n, is_1:
    (string-append (substring v 0 (- (string-length v) (if (not is_1) (v-plurale-n? s n) (+ n 0)))) (v-plurale? v s des))
    )
  )

; Declina il verbo
(define declina             ;         val:
  (lambda (verbo sogg)      ; verbo, sogg:
    (cond
      ((string=? (get-desinenza verbo) "are")(componi verbo sogg "no" 2 #t))
      ((string=? (get-desinenza verbo) "ere")(componi verbo sogg "ono" 3 #f))
      ((string=? (get-desinenza verbo) "ire")(componi verbo sogg "ono" 3 #f))
      (else (nothing verbo))
      )
    )
 )

; Costruisce la frase corretta
(define frase                   ;              val:
  (lambda (sogg verbo ogg)      ; sogg, verbo, ogg:
    (string-append (add-art sogg) " " (declina verbo sogg) " " (add-art ogg))
    )
  )
