(load "99-lisp.scm")
;1维,2维数组            
;数组起始下标为0            
(define (make-array n init) (rep init n))
(define (array-at array n) (element-at array (+ n 1)))
(define (array-replace-at array n new) (replace array new (+ n 1)))

(define (make-array2d width hight init) (make-array hight (make-array width init))) 

(define (array2d-at array2d c r)
	(let ([row (if (> (length array2d) r) (array-at array2d r) '())])
		 (if (null? row) "idx out of range"
			 (if (> c (length row)) "idx out of range"
				(array-at row c)))))
								
(define (array2d-replace-at array2d c r new)
	(let ([row (if (> (length array2d) r) (array-at array2d r) '())])
		 (if (null? row) "idx out of range"
			 (if (> c (length row)) "idx out of range"
				(array-replace-at array2d r (array-replace-at row c new))))))
				
(define (show-matrix matrix)
	(define (show-row row)
		(if (not (null? row)) (begin (display (car row))(display "\n")(show-row (cdr row)))))
	(show-row matrix))						
									   	
(define (snake size)
	(define maxc (* size size))
	(define (snake-imp c matrix cur direction)
		(if (> c maxc) matrix
			(let* ([curx (car cur)]
				   [cury (cadr cur)]
				   [tmpx (+ curx (caar direction))]
				   [tmpy (+ cury (cadar direction))]
                   [newmatrix (array2d-replace-at matrix curx cury c)]
                   [newdirection (if (or ;检测是否需要调整方向
				   				 	 (> 0 tmpx)
                                     (>= tmpx size)
                                     (> 0 tmpy)
                                     (>= tmpy size)
                                     (not (= 0 (array2d-at newmatrix tmpx tmpy)))) (lshift direction 1)
                                     direction)]
                   [newx (+ curx (caar newdirection))]
                   [newy (+ cury (cadar newdirection))])                                                       
            (snake-imp (+ c 1) newmatrix (list newx newy) newdirection))))       
     (snake-imp 1 (make-array2d size size 0)  '(0 0) '((1 0) (0 1) (-1 0) (0 -1)))) 
