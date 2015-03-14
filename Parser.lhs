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

--essentially for convinience, with tokenListToCircuit
listToTokenList :: a -> [b] -> Token a [b] --Note that [b] in Token a [b] is a list of lists of b
listToTokenList a b = Token (a,[b])

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
tokenToCircuitLong :: (Eq b,MonadPlus m) => Token a b -> Circuit b (m a)
tokenToCircuitLong tok = Circuit $ 
	\x -> if checkToken tok x then ( [tokenToCircuit tok] , return $ getToken tok ) else ( [tokenToCircuitLong tok] , mzero )

--Tests for membership repeatedly and returns any successes. 
--note here we count a success being any matched token
tokensToCircuitLong :: (Eq b) => [Token a b] -> Circuit b [a]
tokensToCircuitLong toks = Circuit $
	\x -> ( [tokensToCircuitLong toks] , mapMaybe (\t -> if checkToken t x then Just $ getToken t else Nothing ) $ toks )

tokenToCircuitShort :: (Eq b,MonadPlus m) => Token a b -> Circuit b (m a)
tokenToCircuitShort tok = Circuit $
	\x -> if checkToken tok x then ( [] , return $ getToken tok ) else ( [] , mzero )
	
tokensToCircuitShort :: (Eq b) => [Token a b] -> Circuit b [a]
tokensToCircuitShort toks = Circuit $
	\x -> ( [] , mapMaybe (\t -> if checkToken t x then Just $ getToken t else Nothing ) $ toks )


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




A context sensitive parser
\begin{code}

cParser1Step :: Circuit a (m b)



\end{code}



\begin{code}


data TokenValue = Atom String
	| Number Integer
	| Bool Bool





\end{code}





