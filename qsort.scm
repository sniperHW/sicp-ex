(define my-filter
    (lambda (f l)
        (define cps
            (lambda (l k)
                (if (not (pair? l)) (k '())
                    (let ([h (car l)])
                        (if (f h) (cps (cdr l) (lambda (x) (k (cons h x))))
                            (cps (cdr l) (lambda (x) (k x))))))))
    (cps l (lambda (x) x))))
(define qsort
    (lambda (l)
        (if (not (pair? l)) '()
            (let* ([m (car l)]
                   [large (my-filter (lambda (x) (if (> x m) #t #f)) (cdr l))]
                   [less (my-filter (lambda (x) (if (<= x m) #t #f)) (cdr l))])
            (append (qsort less) (cons m (qsort large)))))))
				  
				    	
