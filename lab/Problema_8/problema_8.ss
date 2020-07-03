;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problema_8) (read-case-sensitive #t) (teachpacks ((lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hanoi.ss" "installed-teachpacks")) #f)))
(define hanoi-moves ; val: lista di coppie
  (lambda (n) ; n > 0 intero
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

(define hanoi-picture
  (lambda (n k)
    (hanoi-picture-rec 1 n 1 0 k)
    )
  )

(define hanoi-picture-rec
  (lambda (d n p t count)
    ( let ((bg (towers-background n) ))
       (if (= count 0)
           (disk-image )
           (above (disk-image) bg))
           
       )
    )
  )