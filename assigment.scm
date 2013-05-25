(begin

(define shhh #f)
(define tell #f)
(let ((secret 0))
  (set! shhh
    (lambda (message)
      (set! secret message)))
  (set! tell
    (lambda ()
      secret))) 


(define lazy
  (lambda (t)
   (let ((val #f) (flag #f))
      (lambda ()
        (if (not flag)
            (begin (set! val (t))
                   (set! flag #t)val)
			 val)
        )))) 


(define (init-instance) 1)
(define get-instance (lazy init-instance))

;创建一个定长数组,维度由dimension定义例如一个2 x 2数组dimension为'(2 2)
(define (make-array init)
	(define (list-ref items n)
	  (if (= n 0)
		  (car items)
		  (list-ref (cdr items) (- n 1))))

	;设置数组元素
	(define (set-array array idx val)
		(define (set-imp array idx val)
			(cond ((>= idx (length array)))
				  ((= idx 0)(set-car! array val))
				  (else (set-imp (cdr array) (- idx 1) val))))
		(if (not (pair? idx)) (set-imp array idx val);一维数组
			;多维数组
			(cond ((= (length idx) 1) (set-imp array (car idx) val))
				  (else (set-array (list-ref array (car idx)) (cdr idx) val))))
	)	  

	;获取数组元素
	(define (get-array array idx)
		(if (not (pair? idx))(list-ref array idx)
			(cond ((= (length idx) 1)(get-array array (car idx)))
				  (else (get-array (list-ref array (car idx)) (car idx)))))
	)
	(let ((data init))
		(lambda (op . arg)
		(cond ((eq? op 'get) (get-array data (car arg)))
			  ((eq? op 'set) (set-array data (car arg) (car (cdr arg))))
			  ((eq? op 'print) data)
			  (else "bad op")))
	)
)	

;(define l_array (make-array (list (list 1 2 3 4) (list 5 6 7 8))))	
;(l_array 'set '(1 1) 10)
;(l_array 'get '(1 1))
;(l_arrau 'print)
)
