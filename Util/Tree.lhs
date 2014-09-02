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

