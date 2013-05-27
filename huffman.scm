(begin

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; sets

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))
(define sample-tree
   (make-code-tree (make-leaf 'A 4)
                   (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))
)

;输入一个序对列表,建立一棵huffman树					
(define (build-huffman pairs)
	(define (build leaf-set)
		(cond ((= (length leaf-set) 1) (car leaf-set))
			  (else 
				(build
					(adjoin-set  
					(make-code-tree (car leaf-set) (car (cdr leaf-set)))
					(cdr (cdr leaf-set))))			
			  ))
	)
	(build (make-leaf-set pairs))
)

(define (encode-symbol symbol tree)
	(define (element-of-set? x set)
	  (cond ((null? set) false)
			((eq? x (car set)) true)
			(else (element-of-set? x (cdr set)))))	
	(define (iter tree ret)
		(cond ((leaf? tree) ret)  ;到达叶节点,返回
			  (else
				(let ((left (left-branch tree))(right (right-branch tree)))
					 (cond ((element-of-set? symbol (symbols left))
							 (iter left (cons 0 ret)))
						   ((element-of-set? symbol (symbols right))
							 (iter right (cons 1 ret)))
						   (else (error "bad symbol"))))		
			  )	
		)			  
	)
	(reverse (iter tree '()))
)  

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;(decode (encode '(A D A B B C A) sample-tree) sample-tree)			  

