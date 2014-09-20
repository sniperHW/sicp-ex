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

;交换列表中的两个元素	      
(define (swap xs n1 n2)
	(let ([a (element-at xs n1)]
		  [b (element-at xs n2)])
		  (reverse (car (cdr (foldl (lambda (acc x)
			(let ([fst (car acc)]
				  [snd (car (cdr acc))])			 
				 (cond [(= fst n1) (list (+ fst 1) (cons b snd))]
					   [(= fst n2) (list (+ fst 1) (cons a snd))]
					   [else (list (+ fst 1) (cons x snd))]))) '(1 ()) xs))))))
					   
(define (bubble xs)
	(define (bubble-imp xs less)	
		(if (= (length xs) 1) (list (car xs) less);返回最大值和剩余值组成的列表
			(let ([f (car xs)]
				  [s (cadr xs)])
				 (if (> f s) (bubble-imp (cdr (swap xs 1 2)) (reverse (cons s less)))
					         (bubble-imp (cdr xs) (reverse (cons f less)))))))
	(define (iter xs result)
		(if (null? xs) result
			(let ([r (bubble-imp xs '())])
				 (iter (cadr r) (cons (car r) result)))))
	(iter xs '()))			 
