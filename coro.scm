(begin
	;一个简单的,用continuation实现的协程接口
	(define current-coro #f);当前获得运行权的coro
	(define (get-current-coro) current-coro)
	(define (set-current-coro! current) (set! current-coro current))
	
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
					   (set-current-coro! (get-from coro))			
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
					(set-current-coro! to)
					(set-context! from k)
					((get-context to) arg)
					arg))))
		 ret)
	)
	
	(define (fun-coro-a arg)
		(define coro-new (make-coro fun-coro-b))
		(display "fun-coro-a\n")
		(switch-to (get-current-coro) coro-new #f)
		(display "coro-a end\n")
		"end"
	)
	
	(define (fun-coro-b arg)
		(display "fun-coro-b\n")
		(display "fun-coro-b end\n")
		"end"
	)
	
	(define (test-coro1)
		(define coro-new (make-coro fun-coro-a))
		(define coro-self (make-coro #f))
		(switch-to coro-self coro-new #f)
	)
	
	(define (fun-coro-a-2 arg)
	(define coro-new (make-coro fun-coro-b-2))
	(define (iter)
		(display "fun-coro-a\n")
		(switch-to (get-current-coro) coro-new #f)
		(iter)
	)
	(iter)
	)
	
	(define (fun-coro-b-2 arg)
		(define coro-self (get-current-coro))
		(define (iter)
			(display "fun-coro-b\n")
			(switch-to  coro-self (get-from coro-self) #f)
			(iter)
		)
		(iter)
	)
	
	(define (test-coro2)
		(define coro-new (make-coro fun-coro-a-2))
		(define coro-self (make-coro #f))
		(switch-to coro-self coro-new coro-new)
	)	
	
)