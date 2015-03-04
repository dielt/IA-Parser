\begin{code}

{-# LANGUAGE TypeFamilies #-}

module Parser where


import Util.Base
import Util.Circuit
import Util.Tree


import Data.Maybe
import Data.Tree
import Control.Monad
--import Data.Foldable

\end{code}

There's a bit of a question of whether we always take single words or if we ever want to parse a sequence of word at a single time.


\begin{code}

newtype Token a b = Token (a,[b]) deriving (Eq,Show)

checkToken :: Eq b => Token a b -> b -> Bool
checkToken (Token (a,b)) c = c `elem` b

checkTokenList :: Eq b => Token a [b] -> Bool
checkTokenList (Token (a,b)) = or . map null $ b

--returns a token with any matched lists
eatTokenList :: Eq b => Token a [b] -> b -> Token a [b] 
eatTokenList (Token (a,b)) c = Token  $
	( a , 
		mapMaybe (\xs -> 
			if null xs 
				then Nothing 
				else if head xs == c 
					then Just $ tail xs 
					else Nothing 
		) $ b
	)


getToken :: Token a b -> a
getToken (Token (a,b)) = a

tokensToForest :: Eq b => [Token a b] -> [b] -> Forest a
tokensToForest _ [] = []
tokensToForest toks (b:bs) = 
	map (\x -> Node x (tokensToForest toks bs) ) .  
	mapMaybe (\tok -> if checkToken tok $ b then Just $ getToken tok else Nothing ) $ toks 


--This makes a circuit which just repeatedly tests for membership.
--Essentially a context free lexer. 
tokenToCircuit :: (Eq b,MonadPlus m) => Token a b -> Circuit b (m a)
tokenToCircuit tok = Circuit $ 
	\x -> if checkToken tok x then ( [tokenToCircuit tok] , return $ getToken tok ) else ( [tokenToCircuit tok] , mzero )


--Tests for membership repeatedly and returns any successes. 
--note here we count a success being any matched token
tokensToCircuit :: (Eq b) => [Token a b] -> Circuit b [a]
tokensToCircuit toks = Circuit $
	\x -> ( [tokensToCircuit toks] , mapMaybe (\t -> if checkToken t x then Just $ getToken t else Nothing ) $ toks )


--This is an analogue to tokenToCircuit not tokens, 
--we take a token with a list of matches and test for membership sequentially, a success is only when all are matched in order
tokenListToCircuit :: (Eq b,MonadPlus m) => Token a [b] -> Circuit b (m a)
tokenListToCircuit tok = Circuit $
	\x -> let tok' = eatTokenList tok x in
		(
		map tokenListToCircuit $ [tok,tok']
		, 
		if (checkTokenList tok || checkTokenList tok') 
			then return $ getToken tok
			else mzero
		)

--similar to the above, an analogue to tokensToCircuit
tokenListsToCircuit :: (Eq b) => [Token a [b]] -> Circuit b [a]
tokenListsToCircuit list = Circuit $
	\x -> let list' = map ((flip eatTokenList) x) list in
		(
		[tokenListsToCircuit $ list' ++ list]
		, 
		mapMaybe (\t -> if checkTokenList t then Just $ getToken t else Nothing) $ list ++ list'
		)



\end{code}





\begin{code}

--any branch of the tree which contains a node which evaluates to false will be snipped off at the nearest joint. 
removeFailedParses :: Eq a => (a ->  Bool) -> Forest a -> Forest a
removeFailedParses test forest = listToForest . filter ( and . map test ) . forestToList $ forest

{-
testTree1 = Node 1 [Node 1 [Node 1 []] , Node 1 [Node 2 [Node 3 []]] ,  Node 1 [Node 2 [Node 1 []]] , Node 2 [Node 2 [Node 2 [ Node 3 []] , Node 3 []] ] ]

testTest1 x = x /= 3
-}
\end{code}








