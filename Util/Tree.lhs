\begin{code}
{-# LANGUAGE TypeFamilies #-}

module Util.Tree where


import Data.Monoid
import Data.Tree
import Control.Monad

\end{code}



This is the class which we can use in our general tree construction

\begin{code}


class TreeAnalogue t where
	type TreeType t :: *
	treeToData :: Tree (TreeType t) -> [String] -> t

--we can use this, in general if we don't want extra arguements.
emptyTreeToData :: (Tree (TreeType t) -> t) -> Tree (TreeType t) -> [String] -> t
emptyTreeToData f t [] = f t
emptyTreeToData f t (x:xs) = undefined -- we could do a nicer error.


 


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
{- --this is way more complicated then I had expected,
-- we essentially need to construct the whole tree all at once.
foldTreeWide :: (a -> b -> b) -> b -> Tree a -> Tree b
foldTreeWide f z tree@(Node x xs) = 
	let
		x' = f x z
		fold' h a (Node y ys) = zip (map getNode ys)
	in

foldTreeWideSub :: (a -> b -> b) -> b -> [Tree a] -> Int -> [Tree b]
foldTreeWideSub f z t depth = 
	let
		inodes = zip [1,2..] $ getForestLevel depth t
		foldr (\(i,(Node y ys)) )
	in
-}



--foldTreeNodeDeepMplus
--foldTree f z

appNode :: (a -> a) -> Tree a -> Tree a
appNode f (Node x xs) = Node (f x) xs 

getNode :: Tree a -> a
getNode (Node x _) = x

getBranch :: Tree a -> [Tree a]
getBranch (Node _ xs) = xs


getForestLevel :: Integral i => i -> Tree a -> [Tree a]
getForestLevel i (Node x xs)
	| i <  1 = undefined --I am increasingly of the opinion that undefined is preferable to silently failing to some default.
	| i == 1 = xs
	| i >  1 = xs >>= getBranch >>= (getForestLevel $ i - 1)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Node x xs) = Node (f x) (map (mapTree f) xs )


\end{code}
