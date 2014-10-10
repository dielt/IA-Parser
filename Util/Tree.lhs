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
treeToList (Node x ts) = map (x :) $ concatMap treeToList ts

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

--some of this stuff is already in appropriate foldable etc libraries

--preserves true
testTree :: (a -> Bool) -> Tree a -> Bool
testTree f t@(Node x xs) = foldr (\a b -> b || (f a)) (f x) $ tail $ flatten t 

--preserves false
testTree' :: (a -> Bool) -> Tree a -> Bool
testTree' f t@(Node x xs) = foldr (\a b -> b && (f a)) (f x) $ tail $ flatten t 

--i.e. z is the default boolean and we return if it ever changes.
testTreeDef :: (a -> Bool) -> Bool -> Tree a -> Bool
testTreeDef f z t = foldTree (\a b -> if (b == not z) then b else f a ) z t  

foldTree :: (a->b->b) -> b -> Tree a -> b
foldTree f z t = foldr f z $ flatten t

--go depth first
foldTreeNodeDeep :: (a -> b -> b) -> b -> Tree a -> Tree b
foldTreeNodeDeep f z (Node x xs) = let x' = f x z in
	Node x' (foldr (\a b -> (foldTreeNodeDeep f z a) : b) [] xs)

--we should note that because we are, in these two functions, 
--preserving intrermediate steps, width vs depth matters
{-
So for this we want to first apply the function to the node
then fold over each sub node, then each subnode of each subnode
-}
--foldTreeWide :: (a -> b -> b) -> b -> Tree a -> Tree b
--foldTreeWide f z tree@(Node x xs) =
		
--foldTreeNodeDeepMplus
--foldTree f z


getNode :: Tree a -> a
getNode (Node x _) = x

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Node x xs) = Node (f x) (map (mapTree f) xs )


\end{code}
