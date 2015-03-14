\begin{code}
{-# LANGUAGE TypeFamilies #-}

module Util.Tree where


import Util.Base

import Data.Maybe
import Data.Monoid
import Data.Tree
import Control.Monad

\end{code}



This is the class which we can use in our general tree construction

\begin{code}


class ForestAnalogue t where
	type ForestType t :: *
	forestToData :: Forest (ForestType t) -> t
	--dataToForest :: t -> Forest (ForestType t) 
	--stringToNode :: [String] -> TreeType t --I get the feeling this should be its own class
	--stringToNode = read . unwords -- this would not be a terrible default cept for type


class TreeAnalogue t where
	type TreeType t :: *
	treeToData :: Tree (TreeType t) -> t
	--dataToTree :: t -> Tree (TreeType t) --note this doesn't work for circuits, essentially the motivating example. 



\end{code}






\begin{code}

--this seems to work as expected
treeToList :: Tree a -> [[a]]
treeToList (Node x []) = [[x]]
treeToList (Node x ts) = map (x :) $ concatMap treeToList ts

forestToList = concatMap treeToList

--This should only really be used as the inverse of listToTree 
-- However note that listToTree . treeToList will join any identical nodes on the same level.
--  i.e., Node 1 [Node 2 [Node 3 []] , Node 2 [Node 4 []]  ] -> Node 1 [Node 2 [ Node 3 [], Node 4 [] ] ]

listToForest :: Eq a => [[a]] -> Forest a
listToForest treeList =
	let
		f ::  Eq a => [a] -> Forest a -> Forest a
		f list forest = 
			if null list 
				then forest 
				else if (head list) `elem` (map getNode forest)
					then foldFilterForest (head list) (\(Node x xs) -> Node x (f (tail list) xs) ) forest
					else ( Node (head list) (f (tail list) []) ) : forest
	in foldr f [] treeList
--
listToTree :: Eq a => [[a]] -> Maybe (Tree a)
listToTree = head' . listToForest


--note we only return results for max depth, thus this differs from similar library fn foldMap.
mapFullTree :: Monoid b => ([a] -> b) -> Tree a -> b
mapFullTree f = mconcat . (map f) . treeToList

mapFullForest :: Monoid b => ([a] -> b) -> [Tree a] -> [b]
mapFullForest = map . mapFullTree

--I shouldn't have to do this again
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
--


--some of this stuff is already in appropriate foldable etc libraries, or more generally in 

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

foldForestNodeDeep :: (a -> b -> b) -> b -> [Tree a] -> [Tree b]
foldForestNodeDeep f z forest = foldr (\tree forest' -> foldTreeNodeDeep f z tree : forest' )  [] forest

--this is much trickier than expected
--foldForestNodeWide :: (a -> b -> b) -> b -> [Tree a] -> [Tree b]
--foldForestNodeWide f z forest = 

appNode :: (a -> a) -> Tree a -> Tree a
appNode f (Node x xs) = Node (f x) xs 

getNode :: Tree a -> a
getNode (Node x _) = x

getBranch :: Tree a -> [Tree a]
getBranch (Node _ xs) = xs

findTree :: Eq a => a -> Forest a -> Maybe (Tree a)
findTree x = foldr (\tree val -> if (getNode tree) == x then Just tree else val) Nothing

--replaces every instance of a tree starting with x with a tree having the function applied
foldFilterForest :: Eq a => a -> (Tree a -> Tree a) -> Forest a -> Forest a
foldFilterForest x f forest = foldr (\tree forest' -> if x == (getNode tree) then (f tree) : forest' else tree : forest'  ) [] forest

getForestLevel :: Integral i => i -> Tree a -> [Tree a]
getForestLevel i (Node x xs)
	| i <  1 = undefined --I am increasingly of the opinion that undefined is preferable to silently failing to some default.
	| i == 1 = xs
	| i >  1 = xs >>= getBranch >>= (getForestLevel $ i - 1)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Node x xs) = Node (f x) (map (mapTree f) xs )

--this really ought to be superseeded by either
nubForest :: Eq a => Forest a -> Forest a
nubForest = listToForest . forestToList

nubTree :: Eq a => Tree a -> Tree a
nubTree (Node x xs) = Node (x) (nubForest xs)


--simply removes any node that fails the test
trimTree ::  (a ->  Bool) -> Tree a -> Maybe (Tree a)
trimTree test (Node x xs) = 
	if test x
		then Just $ Node x (mapMaybe (trimTree test) xs) 
		else Nothing

--original action of trimTree, removes any nodes with nothing in them.
fromJustTree :: Tree (Maybe a) -> Maybe (Tree a)
fromJustTree tree = let tree' = trimTree (not . isNothing) tree in
	if isNothing tree'
		then Nothing
		else Just $ mapTree fromJust (fromJust tree')

trimForest :: (a ->  Bool) -> [Tree a] -> [(Tree a)]
trimForest test = mapMaybe (trimTree test)

--any branch of the tree which contains a node which evaluates to false will be snipped off at the nearest joint. 
trimForestHarsh :: Eq a => (a ->  Bool) -> Forest a -> Forest a
trimForestHarsh test forest = listToForest . filter ( and . map test ) . forestToList $ forest

trimTreeHarsh :: Eq a => (a -> Bool) -> Tree a -> Maybe (Tree a)
trimTreeHarsh test tree = listToTree . filter ( and . map test ) . treeToList $ tree

\end{code}




Zippers and related
\begin{code}

newtype TaggedTree a = TaggedTree (Int,Tree (Integer,a))



\end{code}


