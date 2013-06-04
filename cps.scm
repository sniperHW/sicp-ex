(begin
;continuation passing style
(define retry #f)
(define (factorial-cps x f)
	(if (= x 0) (begin (set! retry f) (f 1))
		(factorial (- x 1) (lambda (v) (f (* v x))))
	)
)

(define product-cps
  (lambda (ls k)
    (let ([break k])
      (let f ([ls ls] [k k])
        (cond
          [(null? ls) (k 1)]
          [(= (car ls) 0) (break 1)]
          [else (f (cdr ls)
                   (lambda (x) 
                     (k (* (car ls) x))))])))))
	 
	 (define (map proc items)
		  (if (null? items)
			  (list)
			  (cons (proc (car items))
					(map proc (cdr items)))))
					
	(define (reciprocals ls)
		(call/cc (lambda (k)
			(map (lambda (x)
					(if (= x 0)
						(k "zero found")
						(/ 1 x))) ls)))
	)
	
	(define (reciprocals-cps ls k)
		(let ((break k))
			(let f ([ls ls] [k k])
				(if (null? ls)(k (list))
					(if (= (car ls) 0)(break "zero found")
						(f (cdr ls) (lambda (x) (k (cons (/ 1 (car ls)) x) )))))
			)
		)
	)
	
)

 