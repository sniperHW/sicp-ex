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

;对xs中的每个元素调用f,如果f返回#t则check-element立刻返回#t,否则到达末尾返回#f		
(define (check-element xs f)
    (call/cc (lambda (break)
        (for-each (lambda (x) (if (f x) (break #t))) xs) 
        #f)))			
		
(define (puzzle size)   
    (define (vaild? queen pos);判断当前位置是否可以放置皇后
        (not (check-element queen (lambda (q) 
								   (let ([x (car q)]
										 [y (cadr q)])
								    (or (= x (car pos)) (= (abs (- x (car pos))) (abs (- y (cadr pos))))))))))	   
    (define (foreach-row x y queen result)
        (cond 
              [(>= x size) result]
              [(>= y size) (cons queen result)]
              [else (let ([newresult (if (vaild? queen (list x y))
                                         (foreach-row 0 (+ y 1) (cons (list x y) queen) result)          
                                         result)])
                          (foreach-row (+ x 1) y queen newresult))]))
    (let ([result (foreach-row 0 0 '() '())])
         (define (show xs)
            (if (not (null? xs))
                (begin (display "------result-------\n")
                (show-matrix (gen-matrix size size (lambda (x y) (if (member? (car xs) (list x y)) '* " "))))
                (show (cdr xs)))))                  
         (show result)
         (display "total solution:")(display (length result))(display "\n")))
