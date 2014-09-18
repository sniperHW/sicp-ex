--判断皇后是否可以合法放置
vaild :: [(Int,Int)] -> (Int,Int) -> Bool
vaild [] _ = True
vaild xs (x,y) = foldr (\(x1,y1) acc -> if (x == x1) || (abs $ x - x1) == (abs $ y - y1) then False else acc) True xs  

foreachrow :: (Int,Int) -> Int -> [(Int,Int)] -> [[(Int,Int)]] -> [[(Int,Int)]]
foreachrow (x,y) size queen result 
	| x >= size = result
	| y >= size = (queen:result)
    | otherwise = let newresult = if vaild queen (x,y) then foreachrow (0,y+1) size ((x,y):queen) result
								  else result
				  in  foreachrow (x+1,y) size queen newresult

puzzle :: Int -> Int
puzzle 0 = 0
puzzle size = length $ foreachrow (0,0) size [] []
