;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problema_8) (read-case-sensitive #t) (teachpacks ((lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hanoi.ss" "installed-teachpacks")) #f)))
(define hanoi-moves ; val: lista di coppie
  (lambda (n)       ; n > 0 intero
    (hanoi-rec n 1 2 3)
    )
  )

(define hanoi-rec ; val: lista di coppie
  (lambda (n s d t) ; n intero, s, d, t: posizioni
    (if (= n 1)
        (list (list s d))
        (let ((m1 (hanoi-rec (- n 1) s t d))
              (m2 (hanoi-rec (- n 1) t d s))
              )
          (append m1 (cons (list s d) m2))
          )
        )
    )
  )

(define hanoi-disk-rec
  (lambda (ls n k s d t)
    (if (= k 0)
        (list (list 1 s) (list 2 d) (list 3 t))
        (cond ((= (caar ls) 1)
               (if (= (cadar ls) 2)
                   (hanoi-disk-rec (cdr ls) n (- k 1) (- s 1) (+ d 1) t)
                   (hanoi-disk-rec (cdr ls) n (- k 1) (- s 1) d (+ t 1))
                   ))
              ((= (caar ls) 2)
               (if (= (cadar ls) 1)
                   (hanoi-disk-rec (cdr ls) n (- k 1) (+ s 1) (- d 1) t)
                   (hanoi-disk-rec (cdr ls) n (- k 1) s (- d 1) (+ t 1))
                   ))
              (else
               (if (= (cadar ls) 1)
                   (hanoi-disk-rec (cdr ls) n (- k 1) (+ s 1) d (- t 1))
                   (hanoi-disk-rec (cdr ls) n (- k 1) s (+ d 1) (- t 1))
                   )
               )
              )
        )
    ))

(define hanoi-disks
  (lambda (n k)
    (hanoi-disk-rec (hanoi-moves n) n k n 0 0)
    )
  )


(define hanoi-pictures-rec         ; Restituisce l'elaborazione grafica di hanoi-disks
  (lambda (ls k ls1 ls2 ls3)  ; ls: lista di interi(mosse), k: numero di mosse
    ; ls1/2/3: liste di dischi presenti su ogni colonna
    (if (= k 0)                 ; mosse esaurite
        (list ls1 ls2 ls3)
        (let ( (fb (caar ls) ) (sb (cadar ls) ) (ln1 (length ls1)) (ln2 (length ls2)) (ln3 (length ls3)) )
          (cond ((= fb 1)  ; parto dalla prima asticella ..
                 (if (= sb 2) ; .. arrivo alla seconda
                     (hanoi-pictures-rec (cdr ls) (- k 1)
                                      (remove (list-ref ls1 (- ln1 1)) ls1) ; cdr
                                      (append ls2 (list (list-ref ls1 (- ln1 1) )) )   ; inserisce come primo elemento 
                                      ls3)
                     ; .. arrivo alla terza
                     (hanoi-pictures-rec (cdr ls) (- k 1)
                                      (remove (list-ref ls1 (- ln1 1)) ls1)
                                      ls2
                                      (append ls3 (list (list-ref ls1 (- ln1 1)) ))
                     )))
                ((= fb 2) ; parto dalla seconda asticella ..
                 (if (= sb 1) ; .. arrivo alla prima
                     (hanoi-pictures-rec (cdr ls) (- k 1)
                                      (append ls1 (list (list-ref ls2 (- ln2 1))) )
                                      (remove  (list-ref ls2 (- ln2 1)) ls2)
                                      ls3)
                     ; .. arrivo alla terza
                     (hanoi-pictures-rec (cdr ls) (- k 1) 
                                      ls1
                                      (remove  (list-ref ls2 (- ln2 1)) ls2)
                                      (append ls3 (list (list-ref ls2 (- ln2 1)) ))
                     )))
                ((= fb 3) ; parto dalla terza asticella..
                 (if (= sb 1) ; .. arrivo alla prima
                     (hanoi-pictures-rec (cdr ls) (- k 1)
                                      (append ls1 (list (list-ref ls3 (- ln3 1)) ))
                                      ls2
                                      (remove  (list-ref ls3 (- ln3 1))
                                                   ls3))
                     ; .. arrivo alla terza
                     (hanoi-pictures-rec (cdr ls) (- k 1)
                                      ls1
                                      (append ls2 (list (list-ref ls3 (- ln3 1)) ) )
                                      (remove  (list-ref ls3 (- ln3 1))
                                               ls3))
                     ))
                ))
        )))

(define hanoi-pictures  ; val 
  (lambda (n k)         
    (hanoi-pictures-rec (hanoi-moves n) k (reverse (list-first n) ) null null)
    )
  )

(define list-first ; val: restituisce la lista della prima colonna all'inizio del problema
  (lambda (n)      ; n: numero di dischi
    (if (= n 0)
        '()
        (append (list-first (- n 1)) (list n))
        )
    )
  )

(define tower-image
  (lambda (ls n k p t)
    (if (null? ls)
        (towers-background 3)
        (if (= (length ls) 1)
            (disk-image (car ls) n p t)
            (above (tower-image (cdr ls) n k p (+ t 1)) (disk-image (car ls) n p t)
                   )
            )
        )
    ))

(define first-tower
  (lambda (n k)
    (tower-image (car (hanoi-pictures n k)) n k 1 0)
    )
  )

(define second-tower
  (lambda (n k)
    (tower-image (cadr (hanoi-pictures n k)) n k 2 0)
    )
  )

(define third-tower
  (lambda (n k)
    (tower-image (caddr (hanoi-pictures n k)) n k 3 0)
    )
  )

(define hanoi-towers ; Produce l'immagine
  (lambda (n k)      ; n,k: interi. n numero di dischi, k mosse
    (let ( (bg (towers-background n) ) ) 
    (cond ((and (= (cadar (hanoi-disks n k)) 0) (= (cadar (cdr (hanoi-disks n k))) 0)) ;controllo per prima e seconda torre
           (above (third-tower n k) bg)
           )
          ((and (= (cadar (hanoi-disks n k)) 0) (= (cadar (cddr (hanoi-disks n k))) 0)) ;controllo per prima e terza
           (above (second-tower n k) bg)
           )
          ((and (= (cadar (cdr(hanoi-disks n k))) 0) (= (cadar (cddr (hanoi-disks n k))) 0 )) ;controllo per secondo e terzo
           (above (first-tower n k) bg)
           )
          ((= (cadar(hanoi-disks n k)) 0)
           (above (above (second-tower n k) (third-tower n k)) bg) ; controllo solo per prima torre
           )
          ((= (cadar (cdr(hanoi-disks n k))) 0)
           (above (above (first-tower n k) (third-tower n k)) bg)
           )
          ((= (cadar (cddr (hanoi-disks n k))) 0 )
           (above (above (first-tower n k) (second-tower n k)) bg)
           )
          (else
           (above (above (above (first-tower n k)(second-tower n k) ) (third-tower n k) ) bg)
           ))
      )
    )
  )