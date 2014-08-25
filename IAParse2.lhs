\begin{code}
module IAParse2 where

import Data.Tree
import Data.Monoid

import IAUtil
import IALexer
import IADataBase
import IAData

\end{code}


We have pretty generic tree functions spread out all over, here and lexer mostly

\begin{code}

--returns all objects in the world matching the name
--or all of whatever datatype is asked for.
findName ::Object c => World -> String -> [c]
findName wrld str = worldFoldFilter wrld ((elem str) . (map fst) . names) (:) []

--this seems to work as expected
treeToList :: Tree a -> [[a]]
treeToList (Node x []) = [[x]]
treeToList (Node x ts) = map (x :) $ concat $ map treeToList ts

--note we only return results for max depth, thus this differs from similar library fn foldMap.
mapFullTree :: Monoid b => ([a] -> b) -> Tree a -> b
mapFullTree f = mconcat . (map f) . treeToList

mapFullForest :: Monoid b => ([a] -> b) -> [Tree a] -> [b]
mapFullForest = map . mapFullTree

\end{code}