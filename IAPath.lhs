
\begin{code}
module IAPath 
(	path
,	lpath
) where

import Data.List
import Data.Maybe

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x
\end{code}

simple A* pathfinder
we need a metric space, also, for now we will also use the cost function as a heuristic
for reference we consider path nodes as a triplet (a,b,c) where a is the parent coord, 
b the current coord, and c the cumulative path cost from start to b
NB as far as I can tell it ought to return an empty list iff there is no valid path from end to start.

\begin{code}
path ::(Eq a,Ord b, Num b) => (a->[a]) -> (a->a->b) -> a -> a ->  [a]
path adj cost start end = if end == start then [] else  path2 adj cost start end [(start,start,0)]

path2 ::(Eq a,Ord b, Num b) => (a->[a]) -> (a->a->b) -> a -> a -> [(a,a,b)] -> [a] 
path2 adj cost start end closedNodes = let newNode = pickOpenNode adj cost end closedNodes in
	if newNode == Nothing 
		then []
		else if (snd3 . fromJust $ newNode) == end
			then reconstructPath closedNodes start (fromJust newNode)  -- map snd3  closedNodes
			else path2 adj cost start end ((fromJust newNode) : closedNodes)

lpath ::(Eq a,Ord b, Num b) => (a->[a]) -> (a->a->b) -> a -> a -> b -> [a]
lpath adj cost start end maxDist = if end == start then [] else  lpath2 adj cost start end maxDist [(start,start,0)]

lpath2 ::(Eq a,Ord b, Num b) => (a->[a]) -> (a->a->b) -> a -> a -> b -> [(a,a,b)] -> [a] 
lpath2 adj cost start end maxDist closedNodes = let newNode = pickOpenNode adj cost end closedNodes in
	if newNode == Nothing 
		then []
		else if (thd3 . fromJust $ newNode) > maxDist
			then []
			else if (snd3 . fromJust $ newNode) == end
				then reconstructPath closedNodes start (fromJust newNode)  -- map snd3  closedNodes
				else lpath2 adj cost start end maxDist ((fromJust newNode) : closedNodes)

reconstructPath :: (Eq a) =>  [(a,a,b)] -> a -> (a,a,b) -> [a]
reconstructPath closedNodes start newNode =  
	if (fst3 newNode) == start
		then [snd3 newNode]
		else (reconstructPath closedNodes start (head $ filter  (\(x,y,z) -> y == (fst3 newNode) )  closedNodes  )) ++ [snd3 newNode]

--nb currently we don't check if a previously chosen node has a lower cost via a later node. 
--As near as I can tell this will be unneeded given how we deal with openNodes
pickOpenNode :: (Eq a,Ord b,Num b) => (a->[a]) -> (a->a->b) -> a  -> [(a,a,b)] -> Maybe (a,a,b)
pickOpenNode adj cost end closedNodes = foldl' pickLowest Nothing openNodes
	where
		pickLowest Nothing x = Just x
		pickLowest (Just u) x = if ( ((cost (snd3 u) end) + (thd3 u)) > ((cost (snd3 x) end) +(thd3 x))) then Just x else Just u
		openNodes = 
			( filter (\(x,y,z) -> and [ x `elem` (map snd3 closedNodes) , not $ y `elem`  (map snd3 closedNodes)  ]) )  . 
			trimPathNodes .
			(map (\(x,y,z) -> (x,y, z + cost x y))) .
			concat . 
			(map (\(x,y,z) -> zip3 (repeat y) (adj y) (repeat z))) $ 
			closedNodes

trimPathNodes ::(Eq a, Ord b) => [(a,a,b)] -> [(a,a,b)]
trimPathNodes nodes = foldl' (\list node -> if (snd3 node) `elem` (map snd3 list) then (foldl' (checkLow node) [] list) else node : list ) [] nodes
	where checkLow currNode newList oldElem = 
		if and [ (snd3 currNode) == (snd3 oldElem) , (thd3 currNode) < (thd3 oldElem) ]
			then currNode : newList
			else oldElem : newList
\end{code}

testing

\begin{code}
aj (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

cs (x,y) (u,v) = (x-u)^2 + (y-v)^2

fm = pickOpenNode aj cs (4,4) [((0,0),(0,0),0),((0,0),(0,1),1),((0,1),(1,1),2),((1,1),(1,2),3),((1,2),(2,2),4)]

fn = pickOpenNode aj cs (4,4) [((0,0),(0,0),0),((0,0),(0,1),1)]

fl =  [((0,0),(0,0),0),((0,0),(0,1),1),((0,1),(1,1),2),((1,1),(1,2),3),((1,2),(2,2),4)]

\end{code}
