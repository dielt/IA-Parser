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


\begin{code}

newtype Token a b = Token (a,[b]) deriving (Eq,Show)

checkToken :: Eq b => Token a b -> b -> Bool
checkToken (Token (a,b)) c = c `elem` b

getToken :: Token a b -> a
getToken (Token (a,b)) = a

listToToken :: Eq b => [Token a b] -> [b] -> Forest a
listToToken _ [] = []
listToToken toks (b:bs) = 
	map (\x -> Node x (listToToken toks bs) ) .  
	mapMaybe (\tok -> if checkToken tok $ b then Just $ getToken tok else Nothing ) $ toks 


--This makes a circuit which just repeatedly tests for membership.
--Essentially a context free lexer. 
tokenToCircuit :: (Eq b,MonadPlus m) => Token a b -> Circuit b (m a)
tokenToCircuit tok = Circuit $ 
	\x -> if checkToken tok x then ( [tokenToCircuit tok] , return $ getToken tok ) else ( [tokenToCircuit tok] , mzero )


--Tests for membership repeatedly and returns all successes.
tokensToCircuit :: (Eq b) => [Token a b] -> Circuit b [a]
tokensToCircuit toks = Circuit $
	\x -> ( [tokensToCircuit toks] , mapMaybe (\t -> if checkToken t x then Just $ getToken t else Nothing ) $ toks )




\end{code}












I am not a fan of any of this, most of it is done better in Util.Tree.lhs

data Lexer a b = Lexer
	{	appLex :: [a] -> Forest b
	,	trimLex :: [b] -> Forest b -> [b]
	}
--

modTrimLexer :: Lexer a b -> ([b] -> Forest b -> [b]) -> Lexer a b
modTrimLexer l f = Lexer (appLex l) f

modAppLexer :: Lexer a b -> ([a] -> Forest b) -> Lexer a b
modAppLexer l f = Lexer f (trimLex l)


--idLexer :: Lexer a a
idLexer1 = Lexer
	{	appLex = \xs -> if null xs then [] else let x = head xs in [Node x (appLex idLexer1 (tail xs))]
	,	trimLex = \list ys -> if null ys then list else let (Node x xs) = head ys in (trimLex idLexer1 (list ++ [x]) xs)
	}

idLexer2 = Lexer
	{	appLex = \xs -> map (\x -> Node x []) xs
	,	trimLex = \list ys -> map (\(Node x xs) -> x) ys
	}

{-
appLex :: ([a] -> ([a],[b])) -> [a] -> Forest b
appLex l xs = let (xs',b) = l xs in 
	map (\a -> Node a (appLex l xs') ) b
--

trimLex :: [b] -> Forest b -> [b]
trimLex parsed forst = 
	let f (Node x xs) = x; 
		nodes = map f forst
	in 
-}
		


