\begin{code}


module Util.Tree where


import Data.Monoid
import Data.Tree
import Control.Monad

\end{code}







\begin{code}

--this seems to work as expected
treeToList :: Tree a -> [[a]]
treeToList (Node x []) = [[x]]
treeToList (Node x ts) = map (x :) $ concat $ map treeToList ts

--note we only return results for max depth, thus this differs from similar library fn foldMap.
mapFullTree :: Monoid b => ([a] -> b) -> Tree a -> b
mapFullTree f = mconcat . (map f) . treeToList

mapFullForest :: Monoid b => ([a] -> b) -> [Tree a] -> [b]
mapFullForest = map . mapFullTree

--I shouldn't have to do this
mapFullTreeM :: MonadPlus m => ([a] -> m b) -> Tree a -> m b
mapFullTreeM f = msum . (map f) . treeToList

mapFullForestM :: MonadPlus m => ([a] -> m b) -> [Tree a] -> [m b]
mapFullForestM = map . mapFullTreeM

unfoldForest2 :: (b -> [(a,b)]) -> b -> [Tree a]
unfoldForest2 f z = map (\(a,b) -> Node {rootLabel=a,subForest= unfoldForest2 f b} ) (f z)

unfoldForest3 ::(b->[([a],b)]) -> b -> [Tree a]
unfoldForest3 f z = concatMap g (f z)
	where
		g ([],b) = []
		g ((x:xs),b) = if null xs 
			then [Node {rootLabel=x,subForest= unfoldForest3 f b} ]
			else [Node {rootLabel=x,subForest=g (xs,b) }]

\end{code}


\begin{code}

--attaches the provided tree to any node equal to the target, note may happen multiple times
attachTree :: (Eq a) => Tree a -> a -> Tree a -> Tree a
attachTree (Node x xs) targ newTree = 
	let 
		xs' = ( map (\ts -> attachTree ts targ newTree) xs )
	in
		if	x == targ
			then Node x (newTree : xs')
			else Node x xs'

--this stuff is already in appropriate foldable etc libraries

--go depth first
foldTreeDeep :: (a -> b -> b) -> b -> Tree a -> Tree b
foldTreeDeep f z (Node x xs) = let x' = f x z in
	Node x' (foldr (\a b -> (foldTreeDeep f z a) : b) [] xs)

foldTreeWide :: (a -> b -> b) -> b -> Tree a -> Tree b
foldTreeWide f z tree@(Node x xs) =
		

getNode :: Tree a -> a
getNode (Node x _) = x
	
--foldTreeMPlus

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Node x xs) = Node (f x) (map (mapTree f) xs )


\end{code}


