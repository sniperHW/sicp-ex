; common procedure
(define (check-element xs f)
    (call/cc (lambda (break)
        (for-each (lambda (x) (if (f x) (break #t))) xs) 
        #f)))	

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
	
(define (rep x n)
	(define (iter result acc)
		(if (= n acc) result
			(iter (cons x result) (+ acc 1))))
	(iter '() 0))
	
(define (swapf xs f)
	(cond [(null? xs) '()]
	      [(not (pair? xs)) '()]
	      [(not (= (length xs) 2)) '()]
	      [else (list (f (car (cdr xs))) (f (car xs)))]))

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

;P03 (*) Find the K'th element of a list.
;The first element in the list is number 1.
;Example:
;* (element-at '(a b c d e) 3)
;C

(define (element-at xs at)
	(define (iter xs acc)
		(cond [(null? xs) "idx out of range"]
			  [(= acc at) (car xs)]
			  [else (iter (cdr xs) (+ acc 1))]))
	(iter xs 1))
			
		
;P04 (*) Find the number of elements of a list.

(define (find xs x)
	(if (check-element xs (lambda (e) (= e x))) x
		(cons "can not find " x)))

;P05 (*) Reverse a list.
(define (my-reverse xs) (foldl (lambda (acc x) (cons x acc)) '() xs))

;P06 (*) Find out whether a list is a palindrome.
;A palindrome can be read forward or backward; e.g. (x a m a x).
(define (palindrome xs) (equal? xs (my-reverse xs)))


;P07 (**) Flatten a nested list structure.
;Transform a list, possibly holding lists as elements into a `flat' list 
;by replacing each list with its elements (recursively).

;Example:
;* (my-flatten '(a (b (c d) e)))
;(A B C D E)
;Hint: Use the predefined functions list and append.

(define (my-flatten xs)
	(if (pair? xs) (foldr (lambda (x acc) (append (my-flatten x) acc)) '() xs)
		(list xs)))
		
;P08 (**) Eliminate consecutive duplicates of list elements.
;If a list contains repeated elements they should be replaced with a single copy 
;of the element. The order of the elements should not be changed.

;Example:
;* (compress '(a a a a b c c a a d e e e e))
;(A B C A D E)

(define (compress xs) 
	(reverse (foldl (lambda (acc x) (if (or (null? acc) (not (eq? (car acc) x))) (cons x acc) acc)) '() xs)))

;P09 (**) Pack consecutive duplicates of list elements into sublists.
;If a list contains repeated elements they should be placed in separate sublists.

;Example:
;* (pack '(a a a a b c c a a d e e e e))
;((A A A A) (B) (C C) (A A) (D) (E E E E))

(define (pack xs)
	(reverse (car (cdr (foldl (lambda (acc x)
					 (let ([fst (car acc)]   ;记录前一个处理值
						   [snd (cadr acc)]) ;分组	   
					  (if (eq? fst x)         
					      (let ([h (car snd)];当前正在处理的分组
								[t (cdr snd)])				  				
						  (list x (cons (cons x h) t)))    ;x与前一个处理值一样,将x添加进当前分组即可
						  (list x (cons (list x) snd)))))  ;x与前一个处理值不一样,创建一个新的x分组
					  '(#f ()) xs)))))						  		
    
;P10 (*) Run-length encoding of a list.
;Use the result of problem P09 to implement the so-called run-length encoding data compression method.
; Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

;Example:
;* (encode '(a a a a b c c a a d e e e e))
;((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

(define (encode xs)
	(let ([p (pack xs)])
		(foldr (lambda (x acc) (cons (list (length x) (car x)) acc)) '() p)))
		
;P11 (*) Modified run-length encoding.
;Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied 
;into the result list. Only elements with duplicates are transferred as (N E) lists.

;Example:
;* (encode-modified '(a a a a b c c a a d e e e e))
;((4 A) B (2 C) (2 A) D (4 E))

(define (encode-modified xs)
	(let ([p (pack xs)])
		(foldr (lambda (x acc)
			(let ([size (length x)])
				(if (> size 1) (cons (list (length x) (car x)) acc)
					(cons (car x) acc)))) '() p)))

;P12 (**) Decode a run-length encoded list.
;Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.

(define (decode xs)
	(foldr (lambda (x acc)
			(if (pair? x) (append (rep (cadr x) (car x)) acc)
				(append (list x) acc))) '() xs))
	
;与P11类似,不过不允许直接使用P9的结果		
;P13 (**) Run-length encoding of a list (direct solution).
;Implement the so-called run-length encoding data compression method directly. 
;I.e. don't explicitly create the sublists containing the duplicates, as in problem P09, 
;but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

;Example:
;* (encode-direct '(a a a a b c c a a d e e e e))
;((4 A) B (2 C) (2 A) D (4 E))

(define (encode-direct xs)
	(reverse (car (cdr (foldl (lambda (acc x)
					 (let ([fst (car acc)]   ;记录前一个处理值
						   [snd (cadr acc)]) ;分组	   
					  (if (eq? fst x)         
					      (let ([h (car snd)];当前正在处理的分组
								[t (cdr snd)])					  				
						  (if (pair? h) (list x (cons (list (+ (car h) 1) x) t))
						  	(list x (cons (cons 2 (list x)) t))))
						  (list x (cons x snd)))))  ;x与前一个处理值不一样,创建一个新的x分组
					  '(#f ()) xs)))))
					  
;P14 (*) Duplicate the elements of a list.
;Example:
;* (dupli '(a b c c d))
;(A A B B C C C C D D)

(define (dupli xs)
	(foldr (lambda (x acc) (append (rep x 2) acc)) '() xs))
	
;P15 (**) Replicate the elements of a list a given number of times.
;Example:
;* (repli '(a b c) 3)
;(A A A B B B C C C)

(define (repli xs n)
	(foldr (lambda (x acc) (append (rep x n) acc)) '() xs))	
	
;P16 (**) Drop every N'th element from a list.
;Example:
;* (drop '(a b c d e f g h i k) 3)
;(A B D E G H K)

(define (drop xs n)
	(reverse (car (cdr (foldl (lambda (acc x)
		    (let ([fst (car acc)]
				  [snd (car (cdr acc))])
			 (if (= (mod fst n) 0) (list (+ fst 1) snd)
				 (list (+ fst 1) (cons x snd))))) '(1 ()) xs)))))		

;P17 (*) Split a list into two parts; the length of the first part is given.
;Do not use any predefined predicates.

;Example:
;* (split '(a b c d e f g h i k) 3)
;( (A B C) (D E F G H I K))

(define (split xs n)
	(swapf (car (cdr (foldl (lambda (acc x)
					 (let ([fst (car acc)]   
						   [snd (cadr acc)])   	   
					 (if (or (= fst 0) (= fst n)) ;开辟新的组
					     (list (+ fst 1) (cons (list x) snd))
					     ;插入原组
					     (let ([h (car snd)];当前正在处理的分组
						       [t (cdr snd)])	
					     (list (+ fst 1) (cons (cons x h) t)))))) '(0 ()) xs)))
			reverse))		     

;P18 (**) Extract a slice from a list.
;Given two indices, I and K, the slice is the list containing the elements between 
;the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.

;Example:
;* (slice '(a b c d e f g h i k) 3 7)
;(C D E F G)

(define (slice xs n1 n2)
	(reverse (car (cdr (foldl (lambda (acc x)
		    (let ([fst (car acc)]
				  [snd (car (cdr acc))])
			 (if (and (>= fst n1) (<= fst n2)) (list (+ fst 1) (cons x snd))
				(list (+ fst 1) snd)))) '(1 ()) xs)))))

;P19 (**) Rotate a list N places to the left.
;Examples:
;* (rotate '(a b c d e f g h) 3)
;(D E F G H A B C)

;* (rotate '(a b c d e f g h) -2)
;(G H A B C D E F)

;Hint: Use the predefined functions length and append, as well as the result of problem P17.					  

(define (rotate xs n)
	(let ([s (if (> n 0) n (+ (length xs) n))])
		(my-flatten (swapf (split xs s) (lambda (x) x)))))
		
;P20 (*) Remove the K'th element from a list.
;Example:
;* (remove-at '(a b c d) 2)
;(A C D)

(define (remove-at xs n)
	(reverse (car (cdr (foldl (lambda (acc x)
		    (let ([fst (car acc)]
				  [snd (car (cdr acc))])
			 (if (= fst n) (list (+ fst 1) snd)
				 (list (+ fst 1) (cons x snd))))) '(1 ()) xs)))))
				 
;P21 (*) Insert an element at a given position into a list.
;Example:
;* (insert-at 'alfa '(a b c d) 2)
;(A ALFA B C D)

(define (insert-at e xs n)
	(reverse (car (cdr (foldl (lambda (acc x)
		    (let ([fst (car acc)]
				  [snd (car (cdr acc))])
			 (if (= fst n) (list (+ fst 1) (cons x (cons e snd)))
				 (list (+ fst 1) (cons x snd))))) '(1 ()) xs)))))

;P22 (*) Create a list containing all integers within a given range.
;If first argument is smaller than second, produce a list in decreasing order.
;Example:
;* (range 4 9)
;(4 5 6 7 8 9)

(define (range b e)
	(define (iter acc result)
		(if (< acc b) result
			(iter (- acc 1) (cons acc result))))
	(iter e '())) 

;P23 (**) Extract a given number of randomly selected elements from a list.
;The selected items shall be returned in a list.
;Example:
;* (rnd-select '(a b c d e f g h) 3)
;(E D A)

(define (rnd-select xs n)
	(define (iter acc result num)
		(if (or (null? num) (>= 0 acc)) result
			(let ([i (+ (random (length num)) 1)])
				 (iter (- acc 1) (cons (element-at num i) result) (remove-at num i)))))
	(iter n '() xs))
	
	
;P24 (*) Lotto: Draw N different random numbers from the set 1..M.
;The selected numbers shall be returned in a list.
;Example:
;* (lotto-select 6 49)
;(23 1 17 33 21 37)	

(define (lotto-select n num)
	(rnd-select (range 1 num) n))
			   		
;P25 (*) Generate a random permutation of the elements of a list.
;Example:
;* (rnd-permu '(a b c d e f))
;(B A D C E F)

;Hint: Use the solution of problem P23.
(define (rnd-permu xs)
	(rnd-select xs (length xs)))
	
	
;P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
;In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there
; are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure 
;mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

;Example:
;* (combination 3 '(a b c d e f))
;((A B C) (A B D) (A B E) ... )	

;组合			   		
(define (combination xs n)	
	(define (iter xs n)
		(cond [(null? xs) '()]
			  [(= n 1) xs]
			  [else (foldr (lambda (x acc) (cons (cons (car xs) (if (pair? x) x (list x))) acc)) '() (iter (cdr xs) (- n 1)))])) 	
	(define (iter2 xs result num)
		(if (< num 0) result
			(iter2 (cdr xs) (append (iter xs n) result) (- num 1))))
	(iter2 xs '() (- (length xs) n)))			   


;排列 	   
(define (arrange xs n)
	(define (gen-swap xs)
		(let ([r (range 2 (length xs))])
			 (foldr (lambda (x acc) (cons (swap xs 1 x) acc)) '() r)))	
	(cond [(null? xs) '()]
		  [(= n 1) xs]		  
		  [else ;(let ([other (foldr (lambda (x acc) (cons (arrange x (- n 1)) acc)) '() (gen-swap xs))])
		  			 (foldr (lambda (x acc) (cons (cons (car xs) (if (pair? x) x (list x))) acc)) '() (arrange (cdr xs) (- n 1)))]));));]))	  
		      