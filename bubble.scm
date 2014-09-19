(define bubble
    (lambda (l)
        (define pass
            (lambda (l left)
                (cond 			
                    [(> (length l) 2) (let ([first (car l)]
                                            [second (cadr l)]
                                            [remain (cddr l)])
                                        (if (< first second) (pass (cons second remain) (cons first left))
                                        (pass (cons first remain) (cons second left))))]
                    [(= (length l) 2) (let ([first (car l)]
                                            [second (cadr l)]) 
                                       (if (< first second) (list (cons first left) second) (list (cons second left) first)))]
                    [else (list left (car l))])))						
        (define iter
            (lambda (l result)
                (if (= (length l) 0) l
                (let* ([passres (pass l '())]
                       [left (car passres)]
                       [max (cdr passres)])
                      (if (= (length left) 0) (append max result)
                          (iter left (append max result)))))))

        (iter l `())))	
