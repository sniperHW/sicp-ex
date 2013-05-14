(begin
	;计算绝对值
	(define (abs x)
	(if (< x 0)
		(- x)
		x))
	;求两数平均值
	(define (average x y)
		(/ (+ x y) 2))
	;判断是否偶数	
	(define (even? n)
	(= (remainder n 2) 0))		
)