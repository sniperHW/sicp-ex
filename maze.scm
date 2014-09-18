(define maze1  '((1 1 1 1 1 1 1 1 1)
				(1 0 1 0 0 0 0 0 1)
				(1 0 1 0 1 0 1 0 1)
				(1 0 1 0 1 0 1 0 1)
				(1 0 0 0 0 0 1 0 1)
				(1 1 1 1 1 1 1 1 1)))

;输入一个(width X hight)的矩阵,(f x y)是一个函数,用于控制x y坐标输出值
(define (gen-matrix width hight f)
	(define (gen-row x y row matrix)
		(if (>= x width) (cons (reverse row) matrix)
			(gen-row (+ x 1) y (cons (f x y) row) matrix)))
	(define (gen y matrix)
		(if (>= y hight) matrix
			(gen (+ y 1) (gen-row 0 y '() matrix))))
	(reverse (gen 0 '())))
	
(define (show-matrix matrix)
	(define (show-row row)
		(if (not (null? row)) (begin (display (car row))(display "\n")(show-row (cdr row)))))
	(show-row matrix))
	
(define (get-matrix-size matrix)
	(if (null? matrix) '()
		(if (null? (car matrix)) '()
			(list (length (car matrix)) (length matrix)))))

(define (member? xs x)
	(cond
		[(null? xs) #f]
		[else (if (equal? x (car xs)) #t (member? (cdr xs) x))]))	

;返回一条路径				
(define (findpath-one maze from to)
	(letrec* ( [direction '((0 -1) (0 1) (-1 0) (1 0))]			
			   [arrive? (lambda (cur) (and (= (car cur) (car to)) (= (cadr cur) (cadr to))))]
			   [moveable?  (lambda (x y)
							 (cond
								[(> y (length maze)) #f]
								[else (let ([line (list-ref maze y)]) 
									   (if (> x (length line)) #f (= (list-ref line x) 0)))]))]
			   [foreach-dir (lambda (dirs pos path close)
							   (cond
								 [(null? dirs) '()]
								 [else (let* ([dir (car dirs)]
											  [dirx (car dir)]
											  [diry (cadr dir)]     
											  [nextpos (list (+ (car pos) dirx) (+ (cadr pos) diry))]
											  [ret (move nextpos path close)])							 
										(if (not (null? ret)) ret (foreach-dir (cdr dirs) pos path close)))]))]
			   [move (lambda (pos path close) 
						(if (arrive? pos) (reverse (cons pos path))
							(if (or (not (moveable? (car pos) (cadr pos))) (member? close pos)) '()
								(foreach-dir direction pos (cons pos path) (cons pos close)))))])
           (cond
		   		[(arrive? from) (list from)]
		   		[(or (not (moveable? (car from) (cadr from))) (not (moveable? (car to) (cadr to)))) '()]
		    	[else (foreach-dir direction from (list from) (list from))])))

;显示一个迷宫,maze为迷宫,path为路径
(define (showmaze maze path)
	(let ([matrix-size (get-matrix-size maze)])
	(define matrix (gen-matrix (car matrix-size) (cadr matrix-size) (lambda (x y)
		(if (member? path (list x y)) '*
			(list-ref (list-ref maze y) x)))))
	(show-matrix matrix))
)

;(showmaze maze1 (findpath-one maze1 '(1 1) '(3 3)))

(define (show pos size)
	(let* ([check-pos (gen-invaild pos size)]
		   [matrix  (gen-matrix size size (lambda (x y) (if (member? check-pos (list x y)) '* " ")))])
	       (show-matrix matrix)))									   


		    	              
