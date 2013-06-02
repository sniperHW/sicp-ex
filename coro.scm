(begin
	;一个简单的,用continuation实现的协程接口
	(define current-coro '());当前获得运行权的coro
	
	;创建coro并返回,fun不会立即执行，由start-run执行
	(define (make-coro fun)
		(define coro (list #f #f))
		(let ((ret (call/cc (lambda (k) (begin
			(set-context! coro k)
			(list 'magic-kenny coro))))))
			(if (and (pair? ret) (eq? 'magic-kenny (car ret)))
				(cadr ret)
				;如果下面代码被执行,则是从switch-to调用过来的
				(begin (let ((result (fun ret)))
					   (set-context! coro #f)
					   (set! current-coro (get-from coro))			
					   ((get-context (get-from coro)) result)));fun执行完成后要回到调用者处
			)
		)
	)
			
	(define (get-context coro) (car coro))
	(define (set-context! coro context) (set-car! coro context))		
	(define (get-from coro) (cadr coro))
	(define (set-from! coro from) (set-car! (cdr coro) from))
	
	(define (switch-to from to arg)
		(let ((ret
			  (call/cc (lambda (k)
					(set-from! to from)
					(set! current-coro to)
					(set-context! from k)
					((get-context to) arg)
					arg))))
		 ret)
	)
	
	;启动一个coro的运行，那个coro将会从它在创建时传入的函数开始运行
	(define (start-run coro . arg)
		(let ((param (if (null? arg) arg (car arg))))
			(if (null? current-coro) (set! current-coro (make-coro #f)))
			(switch-to current-coro coro param))
	)
	
	;将运行权交给另一个coro
	(define (yield coro . arg)
		(let ((param (if (null? arg) arg (car arg))))
			(switch-to current-coro coro param)))
	
	;将运行权还给原来把运行权让给自己的那个coro
	(define (resume . arg)
		(let ((param (if (null? arg) arg (car arg))))
			(switch-to current-coro (get-from current-coro) param)))
	
	(define (fun-coro-a arg)
		(display "fun-coro-a\n")
		(yield (make-coro fun-coro-b))
		(display "coro-a end\n")
		"end"
	)
	
	(define (fun-coro-b arg)
		(display "fun-coro-b\n")
		(display "fun-coro-b end\n")
		"end"
	)
	
	(define (test-coro1)
		(start-run (make-coro fun-coro-a))
	)
	
	(define (fun-coro-a-2 arg)
		(define coro-new (make-coro fun-coro-b-2))
		(define (iter)
			(display "fun-coro-a\n")
			(display (yield coro-new 1))(newline)
			(iter)
		)
		(iter)
	)
	
	(define (fun-coro-b-2 arg)
		(define (iter)
			(display "fun-coro-b\n")
			(display(resume 2))(newline)
			(iter)
		)
		(iter)
	)
	
	(define (test-coro2)
		(start-run (make-coro fun-coro-a-2))
	)
	
)