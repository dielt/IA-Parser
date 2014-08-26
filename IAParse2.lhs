\begin{code}
module IAParse2 where

import Data.Tree
import Control.Monad
import Data.Monoid
import Data.Maybe


import IAUtil
import IALexer
import IADataBase
import IAData
import IASyn

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

--I shouldn't have to do this
mapFullTreeM :: MonadPlus m => ([a] -> m b) -> Tree a -> m b
mapFullTreeM f = msum . (map f) . treeToList

mapFullForestM :: MonadPlus m => ([a] -> m b) -> [Tree a] -> [m b]
mapFullForestM = map . mapFullTreeM

\end{code}

My initial thought is just to use Circuits again, though we would need to be careful about partial application

The idea would be for the circuit applicator to ignore any Intents returned before the whole chain is used up

we might want applyLexer to not msum at the end.

\begin{code}

type Parser = Circuit Token (Maybe Intent)

--note currently due to msum in applyLexers and left catch, 
--the first parser to succed will be the only one.
buildParsedList :: MonadPlus m => [Circuit a (m b)] -> [a] -> m b
buildParsedList parsers token = let x = applyLexers parsers token in x >>= \(intnt,tokens) ->
	if null tokens
		then return intnt
		else mzero


allParsers = [
	parseSys
	]


tokensToIntent :: [Tree Token] -> [Intent]
tokensToIntent tokens = catMaybes $ mapFullForestM (buildParsedList allParsers) tokens


\end{code}

So right now we rely on the order of parsers to heirarchize parses per path through the token tree
However as I do not think we have any sensible way of perfering a given path
I think it best to simply return a list of whatever the each path returned



\begin{code}

parseSys :: World -> Parser
parseSys _ = liftCir $ \inp ->
	case inp of 
		(Action (SysComT x) []) -> Just $ SysCom x
		(Action (SysComT x) ((Name y):ys) ) -> ifM (elem y selfSyn) (Just $ SysCom x)
		_ -> Nothing



\end{code}