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

