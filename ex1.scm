(begin
	(load "util.scm")
	;ex 1.16 ¼ÆËãbµÄÃİ
	(define (_expt product b n maxn)
		(if (= n maxn) product
		 (if (<= (* n n) maxn) (_expt (* product product) b (+ n n) maxn)
		  (_expt (* product b) b (+ n 1) maxn))))
	(define (fast-expt b n)
		(if (<= n 0) 1
			(_expt b b 1 n)))
	;ex 1.11
	(define (ex1-f n)
		(cond ((< n 3) n)
			(else
				(+ (ex1-f (- n 1)) (* 2 (ex1-f (- n 2))) (* 3 (ex1-f (- n 3)))))))
	(define (square x) (* x x))			
	(define (even? n)
	  (= (remainder n 2) 0))				
	(define (expmod base exp m)
	  (cond ((= exp 0) 1)
			((even? exp)
			 (remainder (square (expmod base (/ exp 2) m))
						m))
			(else
			 (remainder (* base (expmod base (- exp 1) m))
						m))))        
;	(define (expmod base exp m)
;		(remainder (fast-expt base exp) m))
	(define (fermat-test n)
	  (define (try-it a)
		(= (expmod a n n) a))
	  (try-it (+ 1 (random (- n 1)))))

	(define (fast-prime? n times)
	  (cond ((= times 0) true)
			((fermat-test n) (fast-prime? n (- times 1)))
			(else false)))
;	(define (sum term a next b)
;	  (if (> a b)
;		  0
;		  (+ (term a)
;			 (sum term (next a) next b))))
	;ex 1.30
	(define (sum term a next b)
		(define (iter a result)
			(if (> a b)
				result
				(iter (next a) (+ (term a) result))))
		(iter a 0))
	;ex 1.31	
	(define (product term a next b)
		(define (iter a result)
			(if (> a b)
				result
				(iter (next a) (* (term a) result))))
		(iter a 1))
	;ex 1.32	
	(define (accumulate combiner null-value term a next b)
		(define (iter a result)
			(if (> a b)
				result
				(iter (next a) (combiner (term a) result))))
		(iter a null-value))
	;ex 1.33
	(define (filtered-accumulate filter combiner null-value term a next b)
		(define (iter a result)
			(if (> a b)
				result
				(if (filter a)
					(iter (next a) (combiner (term a) result))
					(iter (next a) result))))
		(iter a null-value))		

	(define (inc n) (+ n 1))
	(define (cube x) (* x x x))
	(define (sum-cubes a b)
	  (sum cube a inc b))

	(define (identity x) x)
    (define (sum-integers a b)
	  (accumulate + 0 identity a inc b))
	(define (product-integers a b)
	  (accumulate * 1 identity a inc b))

	(define (sum-integers-even a b)
	  (filtered-accumulate even? + 0 identity a inc b))
	(define (product-integers-even a b)
	  (filtered-accumulate even? * 1 identity a inc b))	  
;	(define (sum-integers a b)
;	  (sum identity a inc b))
;	(define (product-integers a b)
;	  (product identity a inc b))	  
)