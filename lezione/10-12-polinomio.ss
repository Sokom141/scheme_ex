(define pol
    (lambda (cs)
        (lambda (x)
           (if (null? (cdr cs))
	       (car cs)
	       (+ (car cs) ((pol (cdr cs)) x))
	       ))
	))
