(begin
	(load "ex2.scm")
	;迷宫
	(define maze  '((1 1 1 1 1 1 1 1 1)
					(1 0 1 0 0 0 1 0 1)
					(1 0 1 0 1 0 1 0 1)
					(1 0 1 0 1 0 1 0 1)
					(1 0 0 0 1 0 0 0 1)
					(1 1 1 1 1 1 1 1 1)))
	(define direction '((0 -1)(0 1)(-1 0)(1 0))) ;上下左右
	(define (get-x-y array-2d x y)
		(list-ref (list-ref array-2d x) y))
	
	(define (test-maze start target)
		(define (is-close cur path);是否已经走过的路	
			(= 1 (accumulate (lambda (pos sum) 
								(if (and (= (car pos) (car cur)) (= (cadr pos) (cadr cur))) (+ sum 1) sum)) 
				 0 path)))
		;检查是否合法路径
		(define (check cur dir path)
			(let ((x (+ (car dir) (car cur)))
				  (y (+ (cadr dir) (cadr cur))))
			 (cond ((is-close (list x y) path) nil)
				   ((or (< x 0) (< y 0)) nil);越界
				   ((= (get-x-y maze x y) 1) nil);阻挡
				   (else (list x y)))))   ;返回下一步合法的坐标
	
		(define (iter cur-step path)
			;(display cur-step)(newline)
			(define (move dir)
				(let ((next (check cur-step (list-ref direction dir) path)))
					(cond ((null? next) nil)
						  (else (iter next (cons cur-step path))))))						  
			(cond ((and (= (car target) (car cur-step)) (= (cadr target) (cadr cur-step))) 
				(cons cur-step path)) ;到达目的地，返回路劲
				  (else
					  (let ((up (move 0))) (cond ((not (null? up)) up)
					  (else (let ((down (move 1))) (cond ((not (null? down)) down)
					  (else (let ((left (move 2))) (cond ((not (null? left)) left)
					  (else (let ((right (move 3))) (cond ((not (null? right)) right)
					  ;无法到达目的地
					  (else nil))))))))))))		
				  )
			)
		)
		(reverse (iter start nil))
	)
)	