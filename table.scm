(begin
	(load "rbtree.scm")
	;表格驱动结构,支持任意多键
	(define (make-table)
		(define (cmp a b)
			(define left (symbol->string a))
			(define right (symbol->string b))
			(cond ((string=? left right)0)
				  ((string<? left right) -1)
				  (else 1)))
		
		(define (put container arg)
				(if (= (length arg) 2)(container 'insert (car arg) (cadr arg))
				(let ((sub-container (container 'find (car arg))))
					(if (null? sub-container)
						(begin (set! sub-container (make-rbtree cmp))
							   (container 'insert (car arg) sub-container)))	
					(put sub-container (cdr arg))))
		)
		
		(define (get table arg)
			(if (or (null? table) (null? arg))'() 
				(if (= 1 (length arg))
					(table 'find (car arg))	
					(get (table 'find (car arg)) (cdr arg)))))
	
		(let ((container (make-rbtree cmp)))
			(lambda (op . arg)	
			(let ((args (if (pair? (car arg))(car arg)arg)))	
			(cond ((eq? op 'put) (put container args))
				  ((eq? op 'get) (get container args))
				  (else "bad op"))))
		)
	)
	
	(define (test-fun1) (display "test-fun1"))
	(define (test-fun2) (display "test-fun2"))
	(define (test-fun3) (display "test-fun3"))
	(define (test-fun4) (display "test-fun4"))
	(define global-table (make-table))
	
	
	(define (put arg . arg-remain)
		(global-table 'put (cons arg arg-remain))
	)
	
	(define (get arg . arg-remain)
		(global-table 'get (cons arg arg-remain))
	)
	
	(global-table 'put 'kenny "kenny")
	(put 'a test-fun1)
	(put 'b 'c test-fun2)
	(put 'd 'e 'f test-fun3)
	(put 'b 'd test-fun4)	
	
)