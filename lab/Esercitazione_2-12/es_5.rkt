;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define av
  (lambda (ls)
      (cond ((null? (cdr ls)) null)
            ((= (+ (car ls) (cadr ls)) 0) (cons 0 (av (cdr ls))))
            ((> (+ (car ls) (cadr ls)) 0) (cons 1 (av (cdr ls))))
            (else (cons -1 (av (cdr ls))))
            )
      )
    )
 