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
	(define (odd? n)
	(= (remainder n 2) 1))
	;求平方
	(define (square x) (* x x))
	;开方
	(define (sqrt x)
	  (define (good-enough? guess)
		(< (abs (- (square guess) x)) 0.00000000000001))
	  (define (improve guess)
		(average guess (/ x guess)))
	  (define (sqrt-iter guess)
		(if (good-enough? guess)
			guess
			(sqrt-iter (improve guess))))
	  (sqrt-iter 1.0))	
)