-- 冒泡排序
bubble :: (Ord a) => [a] -> [a]
bubble []  = error "Can't call pass on an empty list, dummy!"
bubble (x:[]) = [x]
bubble xs = iter xs []
	   where 
		pass xs left		
			| size == 2 = if first < second then (first:left,second)
							 else (second:left,first)
			| size > 2 =  let remain = tail (tail xs) in
						  if first < second then pass (second:remain) (first:left)
							 else pass (first:remain) (second:left)
			| size == 1 = ([],first)				   					 
			where 
				  size  = length xs
				  first = head xs
				  second = head (tail xs)				  	   	   
		iter xs result =
			let 
				passret = (pass xs [])
				left = fst passret
				max = snd passret 
			in 
				if length left == 0 then (max:result)
			else iter left (max:result)		



