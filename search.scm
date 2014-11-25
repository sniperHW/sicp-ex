(define (searchtree tree k)
	(define (check tree exit)
		(if (eq? 'nil tree) 'nil
		     (begin 
		     	(if (eq? (car tree) k) (exit k))
		     	(check (cadr tree) exit)
		     	(check (caddr tree) exit))))
	(call/cc (lambda (exit) (check tree exit))))


;> (searchtree '(k (c (a nil nil) (e (d nil nil) (g nil nil))) (m nil nil)) 'e)
;e
;> (searchtree '(k (c (a nil nil) (e (d nil nil) (g nil nil))) (m nil nil)) 'a)
;a
;> (searchtree '(k (c (a nil nil) (e (d nil nil) (g nil nil))) (m nil nil)) 'f)