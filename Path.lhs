
\begin{code}

module Path (
pathFinder
) where

import Data.Maybe
import Util.Base
\end{code}



A simple, slow, A* pathfinder,
\begin{code}

--Maybe [a] because Just [] means success, no path needed, whereas Nothing means failure
pathFinder :: (Eq a,Num b,Ord b) => (a -> [a]) -> (a -> a -> b) -> (a -> a -> b) -> a -> a -> Maybe [a]
pathFinder adj cost est start end =
	pathFinder2 
		(\(a,b,c) -> foldr (\x list -> (x,a,c + (cost a x)) : list  ) [] (adj a)) est start end [(start,start,0)]

--NB (currentNode,parentNode,CostFromStartToCurrentNode) 

pathFinder2 :: (Eq a,Num b,Ord b) => ((a,a,b) -> [(a,a,b)]) -> (a -> a -> b) -> a -> a -> [(a,a,b)] -> Maybe [a]
pathFinder2 _ _ _ _ [] = Nothing
pathFinder2 f est start end list =
	if checkFst3 end list
		then (findLowThd3 . filter ((end ==) . fst3) $ list) >>= \a -> Just $ reconstructPath start a list []
		else pathFinder2 f est start end ((maybeToList . findLowThd3 . addEstimate end est . filter (\a -> not $ checkFst3 (fst3 a) list) $ (concatMap f list) ) ++ list )


--Maybe [a] because Just [] means success, no path needed, whereas Nothing means failure
pathFinder :: (Eq a,Num b,Ord b) => (a -> [a]) -> (a -> a -> b) -> (a -> a -> b) -> a -> a -> Maybe [a]
pathFinder adj cost est start end =
	pathFinder2 
		(\(a,b,c) -> foldr (\x list -> (x,a,c + (cost a x)) : list  ) [] (adj a)) est start end [(start,start,0)]

--NB (currentNode,parentNode,CostFromStartToCurrentNode) 

pathFinder2 :: (Eq a,Num b,Ord b) => ((a,a,b) -> [(a,a,b)]) -> (a -> a -> b) -> a -> a -> [(a,a,b)] -> Maybe [a]
pathFinder2 _ _ _ _ [] = Nothing
pathFinder2 f est start end list =
	if checkFst3 end list
		then (findLowThd3 . filter ((end ==) . fst3) $ list) >>= \a -> Just $ reconstructPath start a list []
		else pathFinder2 f est start end ((maybeToList . findLowThd3 . addEstimate end est . filter (\a -> not $ checkFst3 (fst3 a) list) $ (concatMap f list) ) ++ list )
		
reconstructPath :: (Eq a,Ord b) => a -> (a,a,b) -> [(a,a,b)] -> [a] -> [a]
reconstructPath start curr list path = 
	let --the fromJust here isn't really justified, but the alternative is awkwardly returning a partial list.
		x = fromJust . findLowThd3 . filter (((snd3 curr) ==) . fst3) $ list		
	in if (fst3 curr) == start
		then path
		else reconstructPath start x list ((fst3 curr) : path)

addEstimate :: (Eq a,Num b) =>  a -> (a -> a -> b) -> [(a,a,b)]  -> [(a,a,b)]
addEstimate end est list = map ( \(a,b,c) -> (a,b,c + (est a end)) ) list

--For testing purposes

costT (x,y) (u,z) = sqrt . fromIntegral $ (x-u)^2+(y-z)^2

adjT (x,y) = filter (\a -> not $ elem a [(0,5),(1,5),(2,5),(3,5),(-1,5),(-2,5),(-3,5)]) [(x+1,y),(x - 1,y),(x,y+1),(x,y - 1),(x+1,y - 1),(x - 1,y - 1),(x+1,y+1),(x - 1,y+1)]

\end{code}



