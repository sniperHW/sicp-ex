import qualified Data.Set as Set
-- 走迷宫
--module Maze   
--( 
--  FindOne   
--) where

--返回指定下标的元素
elemat :: [maybe] -> Int -> maybe			
elemat xs idx = 
		if idx >= length xs then error "index out of range"
		else fetch xs 0
	where fetch (x:xs) acc = 
		if acc == idx then x
		else fetch xs (acc+1)	

-- 检查输入点是否可移动
movable ::[[Int]] -> (Int,Int) -> Bool
movable maze (x,y) =  
		if y < length maze then 
			let line = elemat maze y
			in if x < length line then
				elemat line x == 0
			else False   
		else False
		
-- 输出一条路径
findonepath :: [[Int]] -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
findonepath maze from to
	| not (movable maze from) || not (movable maze to) = []
	| otherwise = foreachdir direction from [from] $ Set.fromList [] 
	where 
		  direction = [(0,-1),(0,1),(-1,0),(1,0)] -- 4个移动方向
		  foreachdir dirs (x,y) path close
			| null dirs = []
		    | otherwise = 
			  let 
					(dirx,diry) = head dirs  
					nextpos = (x+dirx,y+diry) 	
					ret = move path close nextpos
		      in 
					if null ret then
						foreachdir (tail dirs) (x,y) path close
					else ret					
		  move path close (x,y)
			| (x,y) == to = reverse ((x,y):path) --到达目的地 
			| otherwise = 
				if Set.member (x,y) close || not (movable maze (x,y)) then []
				else foreachdir direction (x,y) ((x,y):path) $ Set.insert (x,y) close
					
					
					
									
--[[1,1,1,1,1,1,1,1,1],
--[1,0,1,0,0,0,1,0,1],
--[1,0,1,0,1,0,1,0,1],
--[1,0,1,0,1,0,1,0,1],
--[1,0,0,0,0,0,0,0,1],
--[1,1,1,1,1,1,1,1,1]]				
				
				
--findpath [[1,1,1,1,1,1,1,1,1],[1,0,1,0,0,0,1,0,1],[1,0,1,0,1,0,1,0,1],[1,0,1,0,1,0,1,0,1],[1,0,0,0,0,0,0,0,1],[1,1,1,1,1,1,1,1,1]] (1,1) (3,1)					
				
				
				
