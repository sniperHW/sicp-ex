(begin
	;�������ֵ
	(define (abs x)
	(if (< x 0)
		(- x)
		x))
	;������ƽ��ֵ
	(define (average x y)
		(/ (+ x y) 2))
	;�ж��Ƿ�ż��	
	(define (even? n)
	(= (remainder n 2) 0))
	;��ƽ��
	(define (square x) (* x x))
	;����
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