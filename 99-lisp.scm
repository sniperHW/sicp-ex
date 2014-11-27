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
					   
(define (replace xs x n);将第n个元素替换成x
	(reverse (car (cdr (foldl (lambda (acc xx)
				(let ([fst (car acc)]
					  [snd (car (cdr acc))])
				 (if (= fst n) (list (+ fst 1) (cons x snd))
					 (list (+ fst 1) (cons xx snd))))) '(1 ()) xs)))))
;循环左移动n个元素					 
(define (lshift xs n) (if (> n 0) (lshift (append (cdr xs) (list (car xs))) (- n 1)) xs))
		

;循环右移动n个元素
(define (rshift xs n) (if (> n 0) (rshift (lshift xs (- (length xs) 1)) (- n 1)) xs))

(define (n! n) (if (>= 1 n) 1 (* n (n! (- n 1)))))
	
(define (C n m) (/ (n! n) (* (n! (- n m)) (n! m))))
	
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
	(if (or (> 0 at) (> at (length xs))) "idx out of range" 		  
	(iter xs 1)))
			
		
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
	(cond [(or (null? xs) (< (length xs) n))'()]
		  [(= n 1) (foldr (lambda (x acc) (cons (list x) acc)) '() xs)]
		  [else (append (foldr (lambda (x acc) (cons (cons (car xs) x) acc))
						       '() (combination (cdr xs) (- n 1))) ;取当前(car xs) + (cdr xs)中取n-1个 
				        (combination (cdr xs) n))]));从(cdr xs)中取n个			   
;(length (combination '(1 2 3 4 5 6 7 8 9 10 11 12) 3))

(define (combination-num num n)
	(length (combination (range 1 num) n)))

;排列
(define (permutation xs n)
	(cond [(or (null? xs) (< (length xs) n))'()]
		  [(= n 1) (foldr (lambda (x acc) (cons (list x) acc)) '() xs)]
		  [else (let* ([permutation-n-1 (permutation (cdr xs) (- n 1))];(cdr xs)取n-1的排列
		              [head (car xs)]
		              [permutation-swap (foldl (lambda (acc x) ;将head与arrange-n-1中的元素互换
		                                    (append (foldl (lambda (acc1 xx)
					  							   (append (foldl (lambda (acc2 xxx) (cons (cons (element-at x xx) xxx) acc2)) '() (list (replace x head xx))) acc1))
					  							   '() (range 1 (length x))) acc)) '() permutation-n-1)])
					  (append (append (foldl (lambda (acc x) (cons (cons head x) acc)) '() permutation-n-1) permutation-swap) (permutation (cdr xs) n)))]))
					  
;P27 (**) Group the elements of a set into disjoint subsets.
;a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 
;persons? Write a function that generates all the possibilities and returns them in a list.

;Example:
;* (group3 '(aldo beat carla david evi flip gary hugo ida))
;( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
;... )

;b) Generalize the above predicate in a way that we can specify a list of group sizes and the 
;predicate will return a list of groups.

;Example:
;* (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
;( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
;... )

;Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is 
;the same solution as ((BEAT ALDO) ...). However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) 
;and ((CARLA DAVID) (ALDO BEAT) ...).

;You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
					  							      									 
(define (group xs g)
	(define (tail xs) (car (reverse xs)))	
	;输入xs和n,输出((xs取n的组合1,剩余元素1) (xs取n的组合2,剩余元素2) ...)
	(define (half-group xs n)
		;输出差集
		(define (diffset xs xs1)
			(foldr (lambda (x acc) 
					 (if (not (check-element xs1 (lambda (xx) (eq? x xx))))
						 (cons x acc) acc)) '() xs))
		(let ([c (combination xs n)])
			 (foldr (lambda (x acc)
				 (cons (list x (diffset xs x)) acc)) '() c)))			 			 
	(if (and (not (null? g)) (not (= (length xs) (car g))))
		(let* ([half (half-group xs (car g))])
			  (foldr (lambda (x acc)
						(let ([t (group (tail x) (cdr g))])
							(append (foldr (lambda (x1 acc1)
									(let ([xx (if (= (length g) 1) (list x1) x1)])
									(cons (cons (car x) xx) acc1))) '() t) acc))) '() half))            
		(if (and (not (null? g)) (= (length xs) (car g))) (list (list xs)) (list xs))))
		
		
;P28 (**) Sorting a list of lists according to length of sublists
;a) We suppose that a list contains elements that are lists themselves. 
;The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

;Example:
;* (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
;((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))

;b) Again, we suppose that a list contains elements that are lists themselves. 
;But this time the objective is to sort the elements of this list according to their length frequency; 
;i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a 
;more frequent length come later.

;Example:
;* (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
;((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))

;Note that in the above example, the first two lists in the result have length 4 and 1, 
;both lengths appear just once. The third and forth list have length 3 which appears twice 
;(there are two list of this length). And finally, the last three lists have length 2. This is the most frequent length.

;quick sort
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

;a)
(define (lsort xs) (qsort xs (lambda (l r) (> (length l) (length r)))))

;b)	
(define (lfsort xs)
	(define (statistics xs)
		(foldr (lambda (x acc) (cons (length x) acc)) '() xs))
	(define (get-frequent ftable l)
		(if (= (cadar ftable) (length l)) (caar ftable)
			(get-frequent (cdr ftable) l)))
	(let ([ftable (encode (qsort (statistics xs) (lambda (l r) (> l r))))])
		(qsort xs (lambda (l r)  (> (get-frequent ftable l) (get-frequent ftable r)))))) 
		
;P54A (*) Check whether a given term represents a binary tree
;Write a predicate istree which returns true if and only if its argument is a list representing a binary tree.
;Example:
;* (istree (a (b nil nil) nil))
;T
;* (istree (a (b nil nil)))
;NIL

(define (istree tree)
	(if (or (eq? 'nil tree) (null? tree)) #t ;empty is a tree
		(if (not (= (length tree) 3)) #f;
			(let ([root (not (pair? (car tree)))]
				  [left (istree (cadr tree))]
				  [right (istree (caddr tree))])
			 (and root left right)))))
			 
;P55 (**) Construct completely balanced binary trees
;In a completely balanced binary tree, the following property holds for every node: 
;The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, 
;which means their difference is not greater than one.

;Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes. 
;The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all 
;nodes of the tree.
;Example:
;* cbal-tree(4,T).
;T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
;T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;
;etc......No			 	  
;左右子树节点数量相差不超过1

(define (cbal-tree n)
	(define (iter n result)
		(if (<= n 0) '(nil)
			(let* ([n1 (floor (/ (- n 1) 2))] 
				   [n2 (- (- n 1) n1)]
				   [sub1 (iter n1 result)]
				   [sub2 (iter n2 result)]
				   [r (if (not (equal? sub1 sub2))
						  (foldr (lambda (x1 acc1) 
									(append (foldr (lambda (x2 acc2)
										(cons (list 'x x1 x2) acc2)) '() sub2) acc1))
				                 '() sub1) '() )])            
					(append r (append (foldr (lambda (x1 acc1) 
										(append (foldr (lambda (x2 acc2)
											(cons (list 'x x2 x1) acc2)) '() sub2) acc1))
				   '() sub1) result)))))
	(iter n '()))	                                      

;P56 (**) Symmetric binary trees
;Let us call a binary tree symmetric if you can draw a vertical line through the root 
;node and then the right subtree is the mirror image of the left subtree. Write a predicate 
;symmetric/1 to check whether a given binary tree is symmetric. Hint: Write a predicate mirror/2 
;first to check whether one tree is the mirror image of another. We are only interested in the 
;structure, not in the contents of the nodes.


(define (sametree? tree1 tree2)
	(cond [(or (and (pair? tree1) (not (pair? tree2)))
		       (and (pair? tree2) (not (pair? tree1)))) #f]
		  [(and (eq? 'nil tree1) (eq? 'nil tree2)) #t]     
		  [else (and (sametree? (cadr tree1) (cadr tree2)) (sametree? (caddr tree1) (caddr tree2)))]))
		  
(define (mirror? tree1 tree2);tree1的左子树结构==tree2的右子树且tree1的右子树结构==tree2的左子树则两树是镜像
	(and (sametree? (cadr tree1) (caddr tree2)) (sametree? (caddr tree1) (cadr tree2))))			
		  
(define (symmetric tree) (mirror? (cadr tree) (caddr tree)))


;P57 (**) Binary search trees (dictionaries)
;Use the predicate add/3, developed in chapter 4 of the course, 
;to write a predicate to construct a binary search tree from a list of integer numbers.
;Example:
;* construct([3,2,5,7,1],T).
;T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))

;Then use this predicate to test the solution of the problem P56.
;Example:
;* test-symmetric([5,3,18,1,4,12,21]).
;Yes
;* test-symmetric([3,2,5,7,1]).
;No

(define (construct xs)
	(define (add x tree)
		(if (or (null? tree) (eq? 'nil tree)) (list x 'nil 'nil)
			(if (> x (car tree)) (list (car tree) (cadr tree) (add x (caddr tree)))
				(list (car tree) (add x (cadr tree)) (caddr tree)))))	
	(define (iter xs tree)
		(if (null? xs) tree
			(iter (cdr xs) (add (car xs) tree))))
	(iter xs '()))
			
;P58 (**) Generate-and-test paradigm
;Apply the generate-and-test paradigm to construct all symmetric, 
;completely balanced binary trees with a given number of nodes. Example:
;* sym-cbal-trees(5,Ts).
;Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))] 

;How many such trees are there with 57 nodes? Investigate about how many solutions there are for a given number of nodes? 
;What if the number is even? Write an appropriate predicate.

(define (sym-cbal-trees n)
	(let ([trees (cbal-tree n)])
		 (foldr (lambda (x acc) (if (symmetric x) (cons x acc) acc))
				'() trees)))


;P59 (**) Construct height-balanced binary trees
;In a height-balanced binary tree, the following property holds for every node: 
;The height of its left subtree and the height of its right subtree are almost equal,
; which means their difference is not greater than one.

;Write a predicate hbal-tree/2 to construct height-balanced binary trees for a given height.
;The predicate should generate all solutions via backtracking. Put the letter 'x' as information 
;into all nodes of the tree.
;Example:
;* hbal-tree(3,T).
;T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), t(x, nil, nil))) ;
;T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), nil)) ;
;etc......No
;左右子树高度相差不超过1


(define (hbal-tree h)
	;高度为2的高度平衡树只有
	;   x          x          x
	; x  nil    nil  x     x     x
 	;三种  
	(cond [(= 1 h) (list '(x nil nil))]
		  [(= 2 h) (list '(x (x nil nil) nil) '(x nil (x nil nil)) '(x (x nil nil) (x nil nil)))] 
		  [else (let* ([sub1 (hbal-tree (- h 1))] ;所有高度为h-1的子树
		               [sub2 (hbal-tree (- h 2))] ;所有高度为h-2的子树		  			   
		  			   [t1 (foldr (lambda (x1 acc1) 
		  			   		(append (foldr (lambda (x2 acc2)
		  			   			 (cons (list 'x x2 x1) acc2)) '() sub1) acc1)) '() sub2)]
		  			   [t2 (foldr (lambda (x1 acc1) 
		  					(append (foldr (lambda (x2 acc2)
		  							(cons (list 'x x1 x2) acc2)) '() sub1) acc1)) '() sub2)]
		  			   [t3 (foldr (lambda (x1 acc1) 
		  					(append (foldr (lambda (x2 acc2)
		  						 (cons (list 'x x2 x1) acc2)) '() sub1) acc1)) '() sub1)])		  							
		  			   (append t1 t2 t3))])) 
		  			  		  			    
;构造内节点数量为N的所有高度平衡树
;P60 (**) Construct height-balanced binary trees with a given number of nodes
;Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain?
;Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? This question is more difficult. 
;Try to find a recursive statement and turn it into a predicate minNodes/2 defined as follwos:

;% minNodes(H,N) :- N is the minimum number of nodes in a height-balanced binary tree of height H.
;(integer,integer), (+,?)

;On the other hand, we might ask: what is the maximum height H a height-balanced binary tree with N nodes can have?

;% maxHeight(N,H) :- H is the maximum height of a height-balanced binary tree with N nodes
;(integer,integer), (+,?)

;Now, we can attack the main problem: construct all the height-balanced binary trees with a given nuber of nodes.

;% hbal-tree-nodes(N,T) :- T is a height-balanced binary tree with N nodes.

;Find out how many height-balanced trees exist for N = 15.

(define (exponent x n) 
	    (cond [(= 0 n) 1]
			  [(= 1 n) x]
			  [else (* x (exponent x (- n 1)))]))
			  
(define (maxNodes h) (- (exponent 2 h) 1))

;高度为h的具有最少内节点数量的高度平衡树其两棵子树必定一棵是高度为h-1
;具有最少内节点数量的高度平衡树,一棵是高度为h-2具有最少内节点数量的高度平衡树

;高度为0,最少内节点数量为0,高度为1,最小内节点数量为1,高度为2最少內节点数量为2
;minNodes(h) = 1 ,h == 1
;minNodes(h) = 2 ,h == 2
;minNodes(h) = minNodes( h - 1 ) + minNodes(h - 2) + 根节点,h == 3
;			 = minNodes(2) + minNodes(1) + 1 = 4 
(define (minNodes h)
	(cond [(= 0 h) 0]
		  [(= 1 h) 1]
		  [(= 2 h) 2]
		  [else (+ 1 (+ (minNodes (- h 1)) (minNodes (- h 2))))]))

	
;maxHeight N个节点的高度平衡树的最大高度
;解法1)从h=1开始调用minNodes,如果N >= minNodes(h) and minNodes(h+1) > N ,则h就是最大高度
(define (maxHeight n)
	(define (iter h)
		(if (and (>= n (minNodes h)) (> (minNodes (+ h 1)) n)) h
			(iter (+ h 1))))
	(if (= n 0) 0
		(iter 1)))
		
		
(define (minHeight n) (ceiling (log (+ n 1) 2)))


(define (countNode tree) 
	(if (eq? 'nil tree) 0
		(+ 1 (+ (countNode (cadr tree)) (countNode (caddr tree))))))
	

;hbal-tree-nodes
;解法1)通过maxHeight获得树的最大高度H,通过minHeight获得最小高度h
;通过hbal-tree构造h~H之间的所有高度平衡树,过滤掉节点数量不为n的

(define (hbal-tree-nodes n)
	(let* ([maxh (maxHeight n)]
		   [minh (minHeight n)]
		   [rangeh (range minh maxh)]
		   [all (foldl (lambda (acc x)
				 (append (hbal-tree x) acc)) '() rangeh)])	   	
		   (foldl (lambda (acc x)
		   			(if (= (countNode x) n) (cons x acc) acc))
		   		  '() all))) 
		   		  
		   		  
;P61 (*) Count the leaves of a binary tree
;A leaf is a node with no successors. Write a predicate count-leaves/2 to count them. 

;% count-leaves(T,N) :- the binary tree T has N leaves

(define (count-leaves tree)
	(if (eq? tree 'nil) 0
		(let ([rcount (count-leaves (cadr tree))]
		      [lcount (count-leaves (caddr tree))])
		 (if (and (= 0 rcount) (= 0 lcount)) 1
			 (+ rcount lcount)))))
			 
			 
;P61A (*) Collect the leaves of a binary tree in a list
;A leaf is a node with no successors. Write a predicate leaves/2 to collect them in a list. 
;% leaves(T,S) :- S is the list of all leaves of the binary tree T

(define (leaves tree)
	(if (eq? tree 'nil) '()
		(let ([left (cadr tree)]
		      [right (caddr tree)])
		     (if (and (eq? left 'nil) (eq? right 'nil))
				 (list tree)
			  (append (leaves left) (leaves right))))))
			  
			  
;P62 (*) Collect the internal nodes of a binary tree in a list
;An internal node of a binary tree has either one or two non-empty successors. 
;Write a predicate internals/2 to collect them in a list. 
;% internals(T,S) :- S is the list of internal nodes of the binary tree T.

(define (internals tree)
	(if (eq? tree 'nil) '()
		(let ([left (cadr tree)]
		      [right (caddr tree)])
		     (if (not (and (eq? left 'nil) (eq? right 'nil)))
				 (append (internals left) (internals right) (list (list (car tree) 'nil 'nil)))
				 '()))))
				 
				 
;P62B (*) Collect the nodes at a given level in a list
;A node of a binary tree is at level N if the path from the root to the node has length N-1. 
;The root node is at level 1. Write a predicate atlevel/3 to collect all nodes at a given level in a list. 

;% atlevel(T,L,S) :- S is the list of nodes of the binary tree T at level L

;Using atlevel/3 it is easy to construct a predicate levelorder/2 which creates the level-order sequence 
;of the nodes. However, there are more efficient ways to do that.				 		  	   		  	

(define (atlevel tree l)
	(define (iter tree cur-l)
		(cond [(eq? tree 'nil) '()]
			  [(= l cur-l) (list (list (car tree) 'nil 'nil))]
			  [else (append (iter (cadr tree) (+ cur-l 1)) (iter (caddr tree) (+ cur-l 1)))]))
	(iter tree 1))
	

;广度优先	
(define (levelorder tree)
	(define (travel travel-que result)
		(if (null? travel-que) result
			(let ([mid-result 
			       (foldr (lambda (x acc)
						(if (not (eq? x 'nil))
							(list (cons (cadr x) (cons (caddr x) (car acc)))
							      (cons (car x) (cadr acc)))
							acc)) '(()()) travel-que)])			
				 (append result (travel (car mid-result) (cadr mid-result))))))
	 (travel (list tree) '()))   						
											
;P63 (**) Construct a complete binary tree
;A complete binary tree with height H is defined as follows: The levels 1,2,3,...,H-1 contain the maximum number of nodes 
;(i.e 2**(i-1) at the level i, note that we start counting the levels from 1 at the root). 
;In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted". 
;This means that in a levelorder tree traversal all internal nodes come first, the leaves come second, and empty successors 
;(the nil's which are not really nodes!) come last.

;Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.

;We can assign an address number to each node in a complete binary tree by enumerating the nodes in levelorder,
; starting at the root with number 1. In doing so, we realize that for every node X with address A the following property holds: 
;The address of X's left and right successors are 2*A and 2*A+1, respectively, supposed the successors do exist. 
;This fact can be used to elegantly construct a complete binary tree structure. Write a predicate complete-binary-tree/2 with the following specification: 

;% complete-binary-tree(N,T) :- T is a complete binary tree with N nodes. (+,?)

;Test your predicate in an appropriate way.	

(define (height tree)
	(if (or (null? tree) (eq? tree 'nil)) 0
		(+ 1 (max (height (cadr tree)) (height (caddr tree)))))) 

;判断一棵树是否满二叉树
(define (full-binary-tree? tree)
	(= (countNode tree) (maxNodes (height tree))))


;添加子节点规则
;1) 左右子树节点数量一致往左
;2) 左子树非满往左
;3) 其它情况往右
(define (addNode tree n)
	(if (or (null? tree) (eq? tree 'nil)) (list n 'nil 'nil)
		(let ([left-full (full-binary-tree? (cadr tree))]
		      [left-size (countNode (cadr tree))]
			  [right-size (countNode (caddr tree))])			 
			 (if (or (= left-size right-size) (not left-full)) 						
			    (list (car tree) (addNode (cadr tree) n) (caddr tree))
			    (list (car tree) (cadr tree) (addNode (caddr tree) n))))));往右子树	
			    
(define (complete-binary-tree n)
	(foldl (lambda (acc x)
			 (addNode acc x)) '() (range 1 n)))


;一棵树是完全二叉树的条件
;1) 满二叉树
;2) 左子树是高度为h-1的完全二叉树且右子树是高度为h-2的满二叉树
;3) 左子树是高度为h-1的满二叉树,且右子树是高度为h-1的完全二叉树 

(define (complete-binary-tree? tree)
	(if (full-binary-tree? tree) #t
		(let ([h (height tree)]
		      [h-left (height (cadr tree))]
		      [h-right (height (caddr tree))])
		 (cond [(and (= h-left (- h 1)) (complete-binary-tree? (cadr tree));左子树是高度为h-1的完全二叉树
                     (= h-right (- h 2)) (full-binary-tree? (caddr tree))) #t];右子树是高度为h-2的满二叉树
			   [(and (= h-left (- h 1)) (full-binary-tree? (cadr tree));左子树是高度为h-1的满二叉树   
                     (= h-right (- h 1)) (complete-binary-tree? (caddr tree))) #t];右子树是高度为h-1的完全二叉树 
               [else #f]))))


;P64 (**) Layout a binary tree (1)
;(W,X,Y,L,R) represents a (non-empty) binary tree with root W "positioned" at (X,Y), and subtrees L and R

(define (layout-binary-tree tree)
	(define (layout tree h order)
		(if (eq? tree 'nil) (list 0 'nil)
			(let* ([layout-left (layout (cadr tree) (+ h 1) order)]
			       [self-order (if (= (car layout-left) 0) order (+ (car layout-left) 1))]
			       [layout-right (layout (caddr tree) (+ h 1) (+ self-order 1))]
			       [maxorder (if (= (car layout-right) 0) self-order (car layout-right))])
			       (list maxorder (car tree) self-order h (cdr layout-left) (cdr layout-right)))))
	(cdr (layout tree 1 1 )))				          

;P65 (**) Layout a binary tree (2)
;An alternative layout method is depicted in the illustration opposite. Find out the rules and write the corresponding Prolog predicate.
; Hint: On a given level, the horizontal distance between neighboring nodes is constant.

;Use the same conventions as in problem P64 and test your predicate in an appropriate way. 			        

;最低一层子节点与父节点横坐标差为1,次低层为2,次次低层为4依次类推

(define (layout-binary-tree2 tree)
	(define maxhight (height tree))
	(define hightdelta (append (foldl (lambda (acc x) (cons (exponent 2 x) acc))
							   '() (range 0 (- maxhight 1))) '(0)));层级横坐标数组	
	;layout,如果c为0,表示当前节点的x坐标值尚未确定,需要根据layout-left来确定
	(define (layout tree h c)
		(if (eq? tree 'nil) (list 0 'nil)
			(let* ([layout-left (layout (cadr tree) (+ h 1) 
								        (if (> c 0) 
											(- c (element-at hightdelta (+ h 1))) 
											c))]
			       [self-c (cond [(= 0 c)
			                      (if (= (car layout-left) 0) 1
			                          (+ (car layout-left) (element-at hightdelta (+ h 1))))]
			                     [else c])]
				    [layout-right (layout (caddr tree) (+ h 1) 
								  (+ self-c (element-at hightdelta (+ h 1))))])
				   (list self-c (car tree) self-c h (cdr layout-left) (cdr layout-right)))))  					  					    			    
	(cdr (layout tree 1 0)))

;测试用例	
;(layout-binary-tree2 '(k (c (a nil nil) (e (d nil nil) (g nil nil))) (m nil nil)))
;(layout-binary-tree2 '(c (a nil nil) (e (d nil nil) (g nil nil))))
;(layout-binary-tree2 '(n (k (c (a nil nil) (e (d nil nil) (g nil nil))) (m nil nil)) (u (p nil (q nil nil)) nil)))




;P66 (***) Layout a binary tree (3)
;Yet another layout strategy is shown in the illustration opposite. The method yields a very compact layout while 
;maintaining a certain symmetry in every node. Find out the rules and write the corresponding Prolog predicate. 
;Hint: Consider the horizontal distance between a node and its successor nodes. How tight can you pack together 
;two subtrees to construct the combined binary tree?

;Use the same conventions as in problem P64 and P65 and test your predicate in an appropriate way. 
;Note: This is a difficult problem. Don't give up too early!

;Which layout do you like most?

;1) 如果有左子树和右子树 根节点 = (右子树-左子树)/2 + 左子树
;2) 如果有左子树 根节点 = 左子树 + 1
;3) 如果有右子树 根节点 = c

(define (layout-binary-tree3 tree)
	;用于检测一个节点是否与已经就位的节点产生冲突
	(define (check-collision trees x y)
		(define (match? tree exit)
 			(if (eq? 'nil tree) 'nil
			     (begin
			     	(if (and (eq? x (cadr tree)) (eq? y (caddr tree))) (exit x))
			 	(match? (cadddr tree) exit)    	
			 	(match? (car (cddddr tree)) exit))))
		(define (iter xs exit)
			(if (null? xs) 'nil
			     (begin (match? (car xs) exit)
			     	   (iter (cdr xs) exit))))
		(call/cc (lambda (exit) (iter trees exit))))
	;用于将子树中所有节点x坐标移动2个位置
	(define (shift tree)
		(if (eq? 'nil tree) 'nil
		    (list  (car tree) (+ 2 (cadr tree)) (caddr tree) (shift (cadddr tree)) (shift (car (cddddr tree))))))
	(define (layout tree h c siblings)
		(if (eq? tree 'nil) 'nil
		     (let* ([layout-left (layout (cadr tree) (+ h 1) (if (> c 1) (- c 1) c) siblings)] 
                                          [left-c (if (eq? 'nil layout-left) c (cadr layout-left))] 
                                          [layout-right (layout (caddr tree) (+ h 1) (if (eq? 'nil layout-left) (+ 1 left-c) (+ 2 left-c)) (cons layout-left siblings))]
			[right-c (if (eq? 'nil layout-right) c (cadr layout-right))] 	 
		     	[self-c (if (and (not (eq? 'nil layout-left)) (not (eq? 'nil layout-right))) (+ (/ (- right-c left-c) 2) left-c)
                                     		  (if (eq? 'nil layout-left) c  (+ left-c 1)))]
		     	[self-h (+ h 1)])
		     	(if (not (eq? 'nil (check-collision siblings self-c self-h)))
		     	     ;如果当前节点与已经就位的节点位置产生冲突,则对它的左右子树都调用shift
		                   (list (car tree) (+ self-c 2) self-h  (shift layout-left) (shift layout-right))
		                   (list (car tree) self-c self-h  layout-left layout-right)))))
	(layout tree 0 1 '()))

;测试用例	
;(layout-binary-tree3 '(k (c (a nil nil) (e (d nil nil) (g nil nil))) (m nil nil)))
;(layout-binary-tree3 '(c (a nil nil) (e (d nil nil) (g nil nil))))
;(layout-binary-tree3 '(n (k (c (a nil nil) (e (d nil nil) (g nil nil))) (m nil nil)) (u (p nil (q nil nil)) nil)))
;(layout-binary-tree3 '(a (b nil (c nil nil)) nil))


;P67 (**) A string representation of binary trees
;Somebody represents binary trees as strings of the following type (see example opposite):

;a(b(d,e),c(,f(g,)))

;a) Write a Prolog predicate which generates this string representation, if the tree is given as usual (as nil or t(X,L,R) term). 
;Then write a predicate which does this inverse; i.e. given the string representation, construct the tree in the usual form. Finally, 
;combine the two predicates in a single predicate tree-string/2 which can be used in both directions.

;b) Write the same predicate tree-string/2 using difference lists and a single predicate tree-dlist/2 which does the conversion 
;between a tree and a difference list in both directions.

;For simplicity, suppose the information in the nodes is a single letter and there are no spaces in the string. 

(define (tree->string tree)
	(if (eq? 'nil tree) ""
		(let* ([node (car tree)]
			   [left (cadr tree)]
			   [right (caddr tree)])
			  (if (and (eq? 'nil left) (eq? 'nil right)) node
					(string-append node "(" (tree->string left) "," (tree->string right) ")")))))

;(tree->string '("a" ("b" ("d" nil nil) ("e" nil nil)) ("c" nil ("f" ("g" nil nil) nil))))

(define (string->tree str)
	(define (string-split s)
		(define (mysubstring s start end)
				(cond [(eq? 0 end) ""]
					  [(eq? (string-length s) start) ""]
					  [else (substring s start end)]))
		(define (find-split-index meet-l-bracket meet-r-bracket idx exit)
			(let ([c (string-ref s idx)])
				(cond [(eq? c #\,) 
						(if (or (not meet-l-bracket) meet-r-bracket) (exit idx)
							(find-split-index meet-l-bracket meet-r-bracket (+ idx 1) exit))]
					  [(eq? c #\() (find-split-index #t meet-r-bracket (+ idx 1) exit)]
					  [(eq? c #\)) (find-split-index meet-l-bracket #t (+ idx 1) exit)]
					  [else (find-split-index meet-l-bracket meet-r-bracket (+ idx 1) exit)] 
					  )))
		;(string-split "b(d,e),c(,f(g,))")
		;=>("b(d,e)" "c(,f(g,))")			  
		(let* ([idx (call/cc (lambda (exit) (find-split-index #f #f 0 exit)))]  			  
			   [fst (mysubstring s 0 idx)]		  
			   [snd (mysubstring s (+ idx 1) (string-length s))])		  
			   (list fst snd)))
	(cond [(string=? str "") 'nil]
		  [(eq? (string-length str) 1) (list str 'nil 'nil)]
		  [else (let* ([node (substring str 0 1)] 
					   [splitstr (string-split (substring str 2 (- (string-length str) 1)))]
					   [left-str (car splitstr)]
					   [right-str(cadr splitstr)])
				 (list node (string->tree left-str) (string->tree right-str)))]))

;(string->tree "a(b(d,e),c(,f(g,)))")

(define (tree<->string input)
	(cond [(string? input) (string->tree input)]
		  [(list? input) (tree->string input)]
		  [else 'nil]))
		  
		  
		  					
 					 
;P68 (**) Preorder and inorder sequences of binary trees
;We consider binary trees with nodes that are identified by single lower-case letters, as in the example of problem P67.

;a) Write predicates preorder/2 and inorder/2 that construct the preorder and inorder sequence of a given binary tree, 
;respectively. The results should be atoms, e.g. 'abdecfg' for the preorder sequence of the example in problem P67.

;b) Can you use preorder/2 from problem part a) in the reverse direction; i.e. given a preorder sequence, construct a 
;corresponding tree? If not, make the necessary arrangements.

;c) If both the preorder sequence and the inorder sequence of the nodes of a binary tree are given, then the tree is 
;determined unambiguously. Write a predicate pre-in-tree/3 that does the job.

;d) Solve problems a) to c) using difference lists. Cool! Use the predefined predicate time/1 to compare the solutions.

;What happens if the same character appears in more than one node. Try for instance pre-in-tree(aba,baa,T).	


(define (preorder tree)
	(if (eq? tree 'nil) ""
		(string-append (car tree) (preorder (cadr tree)) (preorder (caddr tree)))))

;(preorder '("a" ("b" ("d" nil nil) ("e" nil nil)) ("c" nil ("f" ("g" nil nil) nil))))

(define (inorder tree)
	(if (eq? tree 'nil) ""
		(string-append (inorder (cadr tree)) (car tree) (inorder (caddr tree)))))
;(inorder '("a" ("b" ("d" nil nil) ("e" nil nil)) ("c" nil ("f" ("g" nil nil) nil))))

;input: preorder sequence
;output: binary tree
;'abdecfg'
(define (build-preorder pre-str)
		(let* ([len (string-length pre-str)]
			   [c (if (> len 0) (substring pre-str 0 1) "")])
			(cond [(eq? len 0) '(nil nil)]
				  [(eq? len 1) (cons c (build-preorder ""))]
				  [(eq? len 2) (list c (build-preorder (substring pre-str 1 2)) 'nil)]
				  [else (list c 
							  (build-preorder (substring pre-str 1 2))
							  (build-preorder (substring pre-str 2 len)))]))) 

;(build-preorder "abdecfg")

;1)寻找根节点,在in-str中寻找pre-str中的第一个元素,此元素即为根节点
;2)将in-str从分成两份例如"dbeacgf"-> "dbe" "cgf"
;3)将pre-str也分成对应数量的两份例如"abdecfg" -> "bde" "cfg"
;4)在两个子串上递归

(define (pre-in-tree pre-str in-str)
	(define (mysubstring s start end)
			(cond [(eq? 0 end) ""]
				  [(eq? (string-length s) start) ""]
				  [else (substring s start end)]))
	(define (find-root c in-str idx exit)
		(if (> idx (string-length in-str)) 'nil
			(if (eq? c (string-ref in-str idx)) (exit idx)
				(find-root c in-str (+ idx 1) exit))))
	(let ([len-pre (string-length pre-str)]
		  [len-in (string-length in-str)])
	(if (not (eq? len-pre len-in)) 'nil
		(cond [(eq? 0 len-pre) 'nil]
			  [(eq? 1 len-pre) (list pre-str 'nil 'nil)]
			  [else (let ([root-idx (call/cc (lambda (exit) (find-root (string-ref pre-str 0) in-str 0 exit)))])
					(if (eq? root-idx 'nil) 'nil
						(let* ([root (substring pre-str 0 1)]
							   [sub-in1 (mysubstring in-str 0 root-idx)]
							   [sub-in2 (mysubstring in-str (+ root-idx 1) (string-length in-str))]
							   [sub-pre1 (mysubstring pre-str 1 (+ 1 (string-length sub-in1)))] 
							   [sub-pre2 (mysubstring pre-str (+ 1 (string-length sub-in1)) (string-length pre-str))]) 	
							   (list root (pre-in-tree sub-pre1 sub-in1)
							   			  (pre-in-tree sub-pre2 sub-in2)))))]))))
;(pre-in-tree "abdecfg" "dbeacgf")

;P69 (**) Dotstring representation of binary trees
;We consider again binary trees with nodes that are identified by single lower-case letters,
;as in the example of problem P67. Such a tree can be represented by the preorder sequence of its 
;nodes in which dots (.) are inserted where an empty subtree (nil) is encountered during the tree traversal. 
;For example, the tree shown in problem P67 is represented as 'abd..e..c.fg...'. 
;First, try to establish a syntax (BNF or syntax diagrams) and then write a predicate tree-dotstring/2 which 
;does the conversion in both directions. Use difference lists.

(define (tree->dotstring tree)
	(if (eq? tree 'nil) "."
		(string-append (car tree) (tree->dotstring (cadr tree)) (tree->dotstring (caddr tree)))))

;(tree-dotstring '("a" ("b" ("d" nil nil) ("e" nil nil)) ("c" nil ("f" ("g" nil nil) nil))))

(define (dotstring->tree dotstr)
	(define (process dotstr)
		(let ([c (substring dotstr 0 1)])
			 (if (string=? "." c) (list (substring dotstr 1 (string-length dotstr)) 'nil)
			  (let* ([left (process (substring dotstr 1 (string-length dotstr)))]
					 [right (process (car left))])
					(list (car right) (list c (cadr left) (cadr right)))))))
	(cadr (process dotstr))) 
			
		
;(dotstring->tree "abd..e..c.fg...")

