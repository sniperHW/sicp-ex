(begin
	(load "ex1.scm")
	;(define (make-rat n d) (cons n d))
	(define (numer x) (car x))
	(define (denom x) (cdr x))

	(define (print-rat x)
	  (display (numer x))
	  (display "/")
	  (display (denom x))
	  (newline))
	;ex 2.1	
	(define (positive? x) (> x 0))	
    (define (negative? x) (< x 0))
	(define (make-rat n d)
		(let ((g (gcd n d)))
		(if (positive? (* n d))
			(cons (/ (abs n) g) (/ (abs d) g))
			(cons (/ (- (abs n)) g) (/ (abs d) g)))))
	;ex 2.2
	(define (make-point x y)
		(cons x y))
	(define (x-point p)(car p))
	(define (y-point p)(cdr p))
	
	(define (print-point p)
		(display "x:")
		(display (x-point p))
		(display " y:")
		(display (y-point p))
		(newline)
	)
	(define (make-segment p1 p2) (cons p1 p2))
	(define (start-segment s)(car s))
	(define (end-segment s)(cdr s))
	(define (print-segment s)
		(display "[start:")
		(display (x-point (start-segment s)))
		(display " ")
		(display (y-point (start-segment s)))
		(display "] ")
		(display "[end:")
		(display (x-point (end-segment s)))
		(display " ")
		(display (y-point (end-segment s)))
		(display "]")
		(newline)		
	)
	;线段长度
	(define (length-segment s)
		(sqrt (+ (square (- (x-point (start-segment s)) (x-point (end-segment s))))
			 (square (- (y-point (start-segment s)) (y-point (end-segment s)))))))
	;线段中点		 
	(define (midpoint-segment s)
		(make-point (average (x-point (start-segment s)) (x-point (end-segment s))) 
		(average (y-point (start-segment s)) (y-point (end-segment s)))))
	;ex 2.3
    ;使用2端点定义矩形
	(define (make-rectangle t-left b-right)
		(if (= (y-point t-left) (y-point b-right))
			(error "can't make a rectangle");如果两点构成的线段平行与x轴则无法构成矩形
			(cons t-left b-right)))
	(define (top-left r) (car r))
	(define (bottom-right r) (cdr r))
	;计算矩形的周长		
	(define (perimeter-rectangle r)
		(* 2
		(+ (abs (- (y-point (top-left r)) (y-point (bottom-right r))))
		   (abs (- (x-point (top-left r)) (x-point (bottom-right r)))))))
	;计算矩形面积
	(define (area-rectangle r)
		(* (abs (- (y-point (top-left r)) (y-point (bottom-right r))))
		   (abs (- (x-point (top-left r)) (x-point (bottom-right r))))))
	;ex 2.4
	(define (cons1 x y)
		(lambda (m) (m x y)))
	(define (car1 z)
		(z (lambda (p q) p)))
	(define (cdr1 z)
		(z (lambda (p q) q)))
	;ex 2.5 看下 2^2 * 3^3的二进制表示,注:序对不能有负数
	(define (cons2 x y) (* (fast-expt 2 x) (fast-expt 3 y)))
	(define (car2 z)
		(define (iter n z)
			(if (= (remainder z 2) 0)
				(iter (+ n 1) (/ z 2))
				 n))
		(iter 0 z))
	(define (cdr2 z)
		(define (iter n z)
			(if (= (remainder z 3) 0)
				(iter (+ n 1) (/ z 3))
				 n))
		(iter 0 z))
	;ex 2.17	
	(define (last-pair p)
		(define (last-pair-imp front back)
			(if (null? back)
				(list front)
				(last-pair-imp (car back) (cdr back))))
		(if (null? p)
			p
			(last-pair-imp (car p) (cdr p))))
	;ex 2.18
	;递归
	(define (reverse p)
		(define (reverse-pair-imp front back)
			(cond ((null? back) (list front))
				  (else (append (reverse-pair-imp (car back) (cdr back)) (list front)))))
		(if (null? p)
			p
			(reverse-pair-imp (car p) (cdr p))))
	;迭代
	(define (reverse2 items)
	  (define (iter things answer)
		(if (null? things)
			answer
			(iter (cdr things) 
				  (cons (car things)
						answer))))
	  (iter items nil))			
	;ex 2.19
	(define us-coins (list 50 25 10 5 1))
	(define uk-coins (list 100 50 20 10 5 2 1 0.5))
	;: (cc 100 us-coins)
	(define no-more? null?)
	(define except-first-denomination cdr)
	(define first-denomination car)
	(define (cc amount coin-values)
	  (cond ((= amount 0) 1)
			((or (< amount 0) (no-more? coin-values)) 0)
			(else
			 (+ (cc amount
					(except-first-denomination coin-values))
				(cc (- amount
					   (first-denomination coin-values))
					coin-values)))))			
	;ex 2.20
	(define (same-parity x . y)
		(define (same-parity-imp checker front back)
			(define (process front) (if (checker front)(list front)nil))	
			(if (null? back)(process front)
				(append (process front) (same-parity-imp checker (car back) (cdr back)))))
		(if (even? x) (same-parity-imp even? x y)
			(same-parity-imp odd? x y)))
	;ex 2.21
	(define (square-list1 items)
		(map square items))
	;ex 2.22
	(define (square-list2 items)
	  (define (iter things answer)
		(if (null? things)
			answer
			(iter (cdr things) 
				  (cons (square (car things))
						answer))))
	  (reverse2 (iter items nil)))
	 ;ex 2.23
	(define (for-each proc items)
	  (define (iter things)
		(cond ((null? things))
			(else
				(proc (car things))
				(iter (cdr things)))))
	 (iter items))
	;ex 2.27
	(define (deep-reverse tree)
		(cond ((null? tree) nil)
			  ((not (pair? tree)) tree)
              (else (reverse (map deep-reverse tree)))))		  
	;ex 2.28
	(define (fringe tree)
		(cond ((null? tree) nil)
			  ((not (pair? tree))(list tree))
			  (else (append (fringe (car tree)) (fringe (cdr tree))))))
	;ex 2.30 使用map
	(define (square-tree tree)
			(cond ((null? tree) nil)
			((not (pair? tree)) (square tree))
            (else (map square-tree tree))))
	;ex 2.30直接定义		
	(define (square-tree2 tree)
		(cond ((null? tree) nil)
			((not (pair? tree)) (square tree))
			(else (cons (square-tree2 (car tree))
						(square-tree2 (cdr tree))))))
	;ex 2.31
	(define (tree-map func tree)
		(define (tree-map-imp tree)
				(cond ((null? tree) nil)
				((not (pair? tree)) (func tree))
				(else (map tree-map-imp tree))))
		(tree-map-imp tree))
	(define (square-tree3 tree) (tree-map square tree))
	;ex 2.32 产生子集合的方式,将集合a分成两部分(front back)
	;a的子集合 = back的子集合 + 将front插入到back的所有子集合中产生的集合 
	(define (subsets s)
	  (if (null? s)
		  (list nil)
		  (let ((rest (subsets (cdr s))))
			(display "rest:")(display rest)(newline)
			(append rest (map (lambda (rest) (cons (car s) rest)) rest)))))
	;ex 2.33
	(define (map2 p sequence)
		(accumulate (lambda (x y) (cons (p x) y)) nil sequence))
	(define (append2 seq1 seq2)
		(accumulate cons seq2 seq1))
	(define (length2 sequence)
		(accumulate (lambda (_ counter) (+ 1 counter)) 0 sequence))
	;ex 2.34
	(define (horner-eval x coefficient-sequence)
	  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
	  		  0
	  		  coefficient-sequence))
	;: (horner-eval 2 (list 1 3 0 5 0 1))
	;ex 2.35
	(define (count-leaves2 t)
		(define (cal node count)
			(if (pair? node) (+ (accumulate cal 0 node) count);当前节点的叶子数+其它兄弟的叶子数
				(+ 1 count))
		)
		(accumulate cal 0 t))
	
	;map fringe将((1 11) 10 (2 (3 4 5 (7 8))))) 展开成 ((1 11) (10) (2 3 4 5 7 8))
	;分别计算每个子表的length累加即可	
	(define (count-leaves3 t)	
		(accumulate (lambda (node counter) (+ (length node) counter)) 0 (map fringe t)))
	;ex 2.36
	(define a_s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
	(define (accumulate-n op init seqs)
	  (if (null? (car seqs))
		  nil
		  (cons (accumulate op init (map car seqs))
				(accumulate-n op init (map cdr seqs)))))
	;ex 2.37
	(define _mat (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
	(define (transpose mat) ;矩阵转置
		(accumulate-n cons nil mat))
	;其余两个略去...
	
	;ex 2.38
	;fold-right和fold-left的主要区别
	;fold-right op会首先被应用到最右边的成员
	;fold-left  op首先被应用到最左边的成员
	;要想op对fold-right和fold-left的任何输入序列都输出相同的结果,op必须满足交换率
	(define fold-right accumulate)
	(define (fold-left op initial sequence)
	  (define (iter result rest)
		(if (null? rest)
			result
			(iter (op result (car rest))
				  (cdr rest))))
	  (iter initial sequence))
	;ex 2.39
	(define (reverse3 sequence)
		(fold-right (lambda (x y) (append y (list x))) nil sequence))
	(define (reverse4 sequence)
		(fold-left (lambda (x y) (cons y x)) nil sequence))
	;ex 2.40
	(define (unique-pairs n)
		(define (process i)
			(map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))
		)
	 (accumulate append nil (map process (enumerate-interval 1 n))))
	(define (prime-sum? pair)
		(prime? (+ (car pair) (cadr pair))))

	(define (make-pair-sum pair)
		(list (car pair) (cadr pair) (+ (car pair) (cadr pair))))	 
	(define (prime-sum-pairs n)
		(map make-pair-sum (filter prime-sum? (unique-pairs n))))
	;ex 2.41
	(define (m-pairs2 n m)
		(define (iter l j ret)
			(cond ((< j 1)
				(list ret))
				(else
				(accumulate
				 append
				 nil
				 (map (lambda (x)
				 	  (cond ((< x j) nil) 
				 			(else (iter (- x 1) (- j 1) (cons x ret)))))
				  	  (enumerate-interval 1 l))))))
		(iter n m nil))		
	;(define (unique-pairs n) (m-pairs n 2))
	;(((1 2)(3 4))(5 6))->((1 2)(3 4)(5 6))
	(define (flat seq)
		(define (iter seq ret)
			(cond ((not (pair? seq)) ret)
				  ((null? (car seq)) (cons nil ret))
				  ((pair? (car seq))
					;可以替换这两行看下区别
					;(let ((ret2 (iter (car seq) ret)))
					;	 (iter (cdr seq) ret2)))					  
					 (let ((ret2 (iter (cdr seq) ret)))
						 (iter (car seq) ret2)))
				  (else (cons seq ret)))		  	
		)
		(iter seq nil)
	)		
	;给定一个列表,从中提取n个所有集合
	;例如(a b c d)->((a b c) (a c d) (a b d) (b c d))
	;这个简单的问题整了我一天多,对函数式语言还是不熟啊
	(define (pick-n seq n)
		;(d-table (1 2 3) 1)->((1 2 3) (2 3) (3))
		;(d-table (1 2 3) 2)->((1 2 3) (2 3))
		;(d-table (1 2 3) 3)->((1 2 3))
		(define (d-table seq n)
			(cond ((= n 0) nil)
				  (else
					(if (<= (length seq) n) (list seq)
						(cons seq (d-table (cdr seq) n ))))))	
		(define (process seq)
			(let ((size (length seq)))
				(cond ((<= n 1) (if (pair? seq) (list (car seq)) seq))
					  ((<= size n) seq)	
					  (else 
						(map (lambda (x)(cons (car seq) x)) (pick-n (cdr seq) (- n 1))))))					  
		)
		(flat (map process (d-table seq n)))
	)
	;更简单的实现
	(define (pick2-n seq n)
		(define (d-table seq n)
			(cond ((= n 0) nil)
				  (else
					(if (<= (length seq) n) (list seq)
						(cons seq (d-table (cdr seq) n ))))))	
		(define (process seq)
			(let ((size (length seq)))
				(cond ((<= n 1) (if (pair? seq) (list (car seq)) seq))
					  ((<= size n) (list seq))	
					  (else 
						(map (lambda (x)
								(if (pair? x)(cons (car seq) x)
									(cons (car seq) (list x)))) (pick2-n (cdr seq) (- n 1))))))					  
		)
		(flatmap process (d-table seq n))
	)
	(define (pick3-n seq n)
		;从n个中选m个->从n-1个中选m个的集合+(将头部取出插入到从n-1个中选m-1个的集合)
		(define (process seq)
			(cond ((<= n 0) (list nil))
				  ((<= (length seq) n) seq)
				  (else
					 (cons (pick3-n (cdr seq) n) (map (lambda (x)(cons (car seq) x)) (pick3-n (cdr seq) (- n 1)))))
				  )
		)
		(flat (process seq))
	)
	(define (unique-pairs2 n) (pick-n (enumerate-interval 1 n) 2))
	;(define (3-pairs n) (pick-n (enumerate-interval 1 n) 3))
	(define (m-pairs n m) (pick-n (enumerate-interval 1 n) m))
	;flatmap练习
	(define (test1 i)
		(define (process x)
			(map (lambda (y) (list x y))(enumerate-interval 1 x)) 
		)
		(flatmap process (enumerate-interval 1 i))
	)
	(define (test2 i j)
		(define (process x)
			(map (lambda (y) (list x y))(enumerate-interval 1 j)) 
		)
		(flatmap process (enumerate-interval 1 i))
	)
	;(flatmap (lambda (x) (map square x)) (list (list 1) (list 2)))
	;2.31引号
	;'后的对象表示对象应该作为数据而不是该求值的表达试对待
	;(accumulate (lambda (x y) (cons (list x) y)) nil ''a)
	;''a 表示一个列表其内容为(quote a)与(list 'quote 'a),'(quote a)等价
	;(cons 'quote 'a)->(quote . a)
	;(cons 'quote (list 'a))-> ''a
	;(accumulate (lambda (x y) (cons (list x) y)) nil (car '('a))
	;'('a)表示列表中有一个元素为('a) (car '('a)) 等价于 ''a
	;比较''a 于 '('a)的区别(car (cdr ''a)) = (car (cdr (car '('a)))) = 'a
	
	;ex 2.56
	
	(define (variable? x) (symbol? x))

	(define (same-variable? v1 v2)
	  (and (variable? v1) (variable? v2) (eq? v1 v2)))

	;(define (make-sum a1 a2) (list '+ a1 a2))

	;(define (make-product m1 m2) (list '* m1 m2))

	(define (sum? x)
	  (and (pair? x) (eq? (car x) '+)))

	(define (addend s) (cadr s))

	(define (augend s) (caddr s))

	(define (product? x)
	  (and (pair? x) (eq? (car x) '*)))

	(define (multiplier p) (cadr p))

	(define (multiplicand p) (caddr p))
	(define (make-sum a1 a2)
	  (cond ((=number? a1 0) a2)
			((=number? a2 0) a1)
			((and (number? a1) (number? a2)) (+ a1 a2))
			(else (list '+ a1 a2))))

	(define (=number? exp num)
	  (and (number? exp) (= exp num)))

	(define (make-product m1 m2)
	  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
			((=number? m1 1) m2)
			((=number? m2 1) m1)
			((and (number? m1) (number? m2)) (* m1 m2))
			(else (list '* m1 m2))))
	;用base^expon表示base的expon次幂
	(define (make-exponentiation base expon)
		(if (number? expon)
			(cond ((= expon 0) 1)
				  ((= expon 1) base)	
				  (else (list '^ base expon)))
			(list '^ base expon))	  
	)
	(define make-exp make-exponentiation)
	(define (exponentiation? e)
		(if (pair? e)(eq? (car e) '^)#f))
	(define is-exp? exponentiation?)
	(define (base e)
		(if (not is-exp?) (error "e is not a exponentiation")
			(car (cdr e))))			
	(define (exponent e);获得指数
		(if (not is-exp?) (error "e is not a exponentiation")
			(car (cddr e))))		
	(define (exponent-dec e);指数-1
		(let ((expon (exponent e)))
			(if (number? expon) (make-exp (base e) (- expon 1))
				(make-exp (base e) (list '- expon 1)))))	
	(define (deriv exp var)
	  (cond ((number? exp) 0)
			((variable? exp)
			 (if (same-variable? exp var) 1 0))
			((sum? exp)
			 (make-sum (deriv (addend exp) var)
					   (deriv (augend exp) var)))
			((product? exp)
			 (make-sum
			   (make-product (multiplier exp)
							 (deriv (multiplicand exp) var))
			   (make-product (deriv (multiplier exp) var)
							 (multiplicand exp))))
			;对幂的处理
			((is-exp? exp)
			  (make-product (make-product (exponent exp) (exponent-dec exp)) 
			  (deriv (base exp) var))	
			)
			(else
			 (error "unknown expression type -- DERIV" exp))))
	
	;ex 2.57
	(define (augend s)
		(if (> (length (cddr s)) 1) (append (list '+) (cddr s))
			(caddr s)))
	(define (multiplicand p)
			(if (> (length (cddr p)) 1) (append (list '*) (cddr p))
			(caddr p)))
	;ex 2.58		
	;中缀表示
	(define (sum? x)
	  (and (pair? x) (eq? (cadr x) '+)))
	(define (addend s) (car s))
	(define (augend s)		
		(if (> (length (cddr s)) 1) (cddr s)
			(caddr s)))

	(define (product? x)
	  (and (pair? x) (eq? (cadr x) '*)))
	(define (multiplier p) (car p))
	(define (multiplicand p)			
		(if (> (length (cddr p)) 1) (cddr p)
			(caddr p)))
	
	(define (make-sum a1 a2)
	  (cond ((=number? a1 0) a2)
			((=number? a2 0) a1)
			((and (number? a1) (number? a2)) (+ a1 a2))
			(else (list a1 '+ a2))))

	(define (make-product m1 m2)
	  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
			((=number? m1 1) m2)
			((=number? m2 1) m1)
			((and (number? m1) (number? m2)) (* m1 m2))
			(else (list m1 '* m2))))	
	;(deriv '(x * y * (x + 3)) 'x)
	;(deriv '(x + 3 * (x + y + 2))	
)