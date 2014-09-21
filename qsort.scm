	(define (foldl f init xs)
		(define (iter xs acc)
			(if (null? xs) acc
				(iter (cdr xs) (f acc (car xs)))))
		(iter xs init))
	
	(define (foldr f init xs)
		(define (iter xs acc)
			(if (null? xs) acc
				(iter (cdr xs) (f (car xs) acc))))
		(iter (reverse xs) init))
	
	(define (qsort l greater)
		(if (not (pair? l)) '()
			(let ([m (car l)]
			     [partition (foldr (lambda (x acc)
					         (let ([small (car acc)]
						       [large (cadr acc)])
						 (if (greater x m) (list small (cons x large))
						     (list (cons x small) large))))
					'(()()) (cdr l))])  
			(append (qsort (car partition) greater) 
			        (cons m (qsort (cadr partition) greater))))))
				  
				    	
