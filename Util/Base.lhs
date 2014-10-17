\begin{code}


module Util.Base where


import Data.Maybe
import Data.Monoid
import Data.Tree
import Data.List
import Control.Monad
import Control.Monad.Trans.State
import qualified Control.Category as C
import Control.Monad
import Control.Applicative

\end{code}

We can alway split this up into seperate files
e.g. IAStateT IACircuit or similar

Basic utility stuff,

\begin{code}
infixr 8 .:
(.:) :: (c->d) ->(a->b->c) -> (a->b->d)
(.:) = (.) . (.)

infixr 8 .::
(.::) :: (d->e) -> (a->b->c->d) -> (a->b->c->e)
(.::) = (.) . (.) . (.)

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

--sort of like guard or when but with values
ifM :: MonadPlus m => Bool -> m a -> m a
ifM True  x = x
ifM False _ = mzero 


uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 f (a,b,c,d) = f a b c d


--I don't think the type system will allow for a generalized version
eat1Arg f = \a -> f
eat2Arg f = \a b -> f
eat3Arg f = \a b c -> f
eat4Arg f = \a b c d -> f
eat5Arg f = \a b c d e -> f
eat6Arg f = \a b c d e f-> f


head' = listToMaybe
tail' xs = if null xs then Nothing else Just $ tail xs

appHead _ [] = []
appHead f (x:xs) = (f x) : xs

appTail _ [] = []
appTail f (x:xs) = x : (map f xs)

--again I assume that there is a prelude function for this
deleteAll :: Eq a => a -> [a] -> [a]
deleteAll = filter . (/=)


\end{code}


Some more specialized functions for dealing with lists

\begin{code}

listFstFilter :: [(Bool,a)] -> [a]
listFstFilter xs = foldr (\x list -> if fst x then snd x : list else list ) [] xs

--based on the last of the tuple
findLowSnd2 :: Ord b => [(a,b)] -> Maybe (a,b)
findLowSnd2 [] = Nothing
findLowSnd2 (x:xs) = Just $ foldr (\a b -> if (snd a) <= (snd b) then a else b ) x xs
 
findHighSnd2 :: Ord b => [(a,b)] -> Maybe (a,b)
findHighSnd2 [] = Nothing
findHighSnd2 (x:xs) = Just $ foldr (\a b-> if snd a > snd b then a else b) x xs

findLowThd3 :: Ord c => [(a,b,c)] -> Maybe (a,b,c)
findLowThd3 [] = Nothing
findLowThd3 (x:xs) = Just $ foldr (\a b -> if (thd3 a) <= (thd3 b) then a else b ) x xs


checkFst3 :: Eq a => a -> [(a,b,c)] -> Bool
checkFst3 a list = or $ map ((a ==) . fst3) list
checkSnd3 :: Eq b => b -> [(a,b,c)] -> Bool
checkSnd3 a list = or $ map ((a ==) . snd3) list
checkThd3 :: Eq c => c -> [(a,b,c)] -> Bool
checkThd3 a list = or $ map ((a ==) . thd3) list

\end{code}


\begin{code}

listOr a b = if null a then b else a

emptyList :: a -> [a]
emptyList a = tail [a]

\end{code}







\begin{code}

--forces a function to act per line on streaming textual input
eachLine :: (String -> String) -> (String -> String)
eachLine f = unlines . map f . lines

\end{code}







Merged stuff

\begin{code}

--stuff below should probably go into some sort of parser yModule

formatNames :: [String] -> String
formatNames [] = ""
formatNames (x:xs) = let
		counter stuff def = foldr (\a b -> if a == def then b+1 else b ) 1 stuff
		count =  counter xs x
		remaining = deleteAll x xs
	in
		(if count > 1
			then (show count) ++ " " ++ x ++ "s"
			else if (head x) `elem` upperCaseLetters
				then x
				else if (head x) `elem` vowels
					then "an " ++ x
					else "a " ++ x 
	) ++ (
		if null remaining
			then "" 
			else if null . deleteAll (head remaining) . tail $ remaining
				then ", and " 
				else ", "
		)  ++ (formatNames remaining)


upperCaseLetters :: [Char]
upperCaseLetters =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZ"

vowels :: [Char]
vowels =
	"aeoiuAEOIU"

\end{code}


Some neat number theory stuff, 
This is mostly not well suited to computation however.

\begin{code}

relPrime = (1 ==) .: gcd

--removes any numbers which have a common factor which had appeared previously in the list
relPrimes :: [Integer] -> [Integer]
relPrimes = reverse . (foldl' f [])
	where 
		f out inp = if  and . (map (relPrime inp)) $ out
			then inp : out
			else out

relPrimesCheck :: Integer -> [Integer] -> Bool
relPrimesCheck n list = n == ( last . relPrimes $ list ++ [n] )

powerList = appNat (^)
multList = appNat (*)

--powerListPM = appInt (^) --turns out haskell doensn't like negative exponents.
multListPM = appInt (*)


appNat :: (Integral b) => (a->b->c) -> a -> [c]
appNat f x = foldr (\a b ->  (f x a) : b ) [] [1,2..]

appInt :: (Integral b) => (a->b->c) -> a -> [c]
appInt f x = foldr (\a b -> (f x a) : b ) [] (alternateLists [1,2..] [-1,-2..])

alternateLists :: [a] -> [a] -> [a]
alternateLists [] list2 = []
alternateLists list1 list2 = (head list1) : (alternateLists list2 (tail list1))

--generates any multiples
genMult :: [Integer] -> (Integer,Integer) -> [Integer]
genMult list (top,bottom) = (deleteAll 1 list) >>= (takeWhile (\a -> top > a && bottom < a)) . multListPM

--Valid for monotone increasing lists and assumes we only want positive multiples.
genMultMon list' top = let list = takeWhile (top >) list' in
	(deleteAll 1 list) >>= (takeWhile (top >)) . multList

eulPhiFn :: Integer -> Integer
eulPhiFn n = fromIntegral . length $
	[x| x <- [1,2..n] , (gcd x n) == 1]

--modEquiv :: Integer -> Integer -> Integer -> Bool
modEquiv m a b = mod a m == mod b m

{- not working right
powerDiv a b =
	let f x y = 
		if x `rem` y == 0 
			then f x (y*b)
			else y `quot` b
	in
	if a `rem` (f a b) == 0
		then Just $ f a b
		else Nothing
-}


--haha
primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

primeFact :: Integral a => a -> [a]
primeFact n = foldr f [] primes
	where
		f a list = undefined
	




\end{code}