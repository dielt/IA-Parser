
\begin{code}

module IAUtil where


import Data.Maybe
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


--I don't think the type system will allow for a generalized version
eat1Arg f = \a -> f
eat2Arg f = \a b -> f
eat3Arg f = \a b c -> f
eat4Arg f = \a b c d -> f


head' = listToMaybe
tail' xs = if null xs then Nothing else Just $ tail xs

appHead _ [] = []
appHead f (x:xs) = (f x) : xs

listFstFilter :: [(Bool,a)] -> [a]
listFstFilter xs = foldr (\x list -> if fst x then snd x : list else list ) [] xs


--again I assume that there is a prelude function for this
deleteAll :: Eq a => a -> [a] -> [a]
deleteAll = filter . (/=)

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

	
  
--forces a function to act per line on streaming textual input
eachLine :: (String -> String) -> (String -> String)
eachLine f = unlines . map f . lines




\end{code}





All of our various stateT utility code, 
there are probably library equivilents to some of these
\begin{code}


stateToStateT :: Monad m => State a b -> StateT a m b
stateToStateT s = StateT $ \a -> return $ runState s a

--we plan to use this for type
--fn :: StateT World IO World -> StateT World IO ()
--But it is more generally of type
stateTMerger :: Monad m => StateT a m a -> StateT a m ()
stateTMerger = mapStateT (\a-> a >>= \a' -> return ((),fst a') )

stateTFnMerger :: Monad m => (b -> a) -> StateT a m b -> StateT a m ()
stateTFnMerger = stateTMerger .: liftM

stateTMonadLift :: Monad m => m b -> StateT a m b
stateTMonadLift = stateTJoin . return --equiv to, fnToStateT . const

fnToStateT :: Monad m => (a -> m b) -> StateT a m b
fnToStateT f = 
	StateT $
		\s -> f s >>= (
			\mb -> return (mb,s) 
		)

--And similarly
stateTMergerJoin :: Monad m => StateT a m (m a) -> StateT a m ()
stateTMergerJoin = stateTMerger . stateTJoin -- mapStateT (\a-> (join $ liftM fst a) >>= \a' -> return ((),a') )

stateTJoin :: Monad m => StateT a m (m b) -> StateT a m b
stateTJoin = mapStateT (\a -> 
	do
		a' <- (join $ liftM fst a) 
		b' <- liftM snd a
		return (a',b')
	)

\end{code}







Basic Circuit code


nb. chainCir == (.) from Category, Thus
As near as I can tell: (liftCir f) `chainCir` (liftCir g) == liftCir (f . g)
Which means: (liftCir id) `chainCir` (liftCir f) == liftCir (id f) == liftCir f
I.e. (liftCir id) forms the identity of the monoid over chainCir


\begin{code}


newtype Circuit a b = Circuit 
	{unCircuit :: a -> ([Circuit a b], b)
	}
--preliminary arr def
liftCir :: (a -> b) -> Circuit a b
liftCir f =  Circuit $ \a -> ([],f a)

--so First arguement is the first string
liftCir2 :: MonadPlus m => (a -> a -> (m b)) -> Circuit a (m b)
liftCir2 f  = Circuit $ \a -> ([liftCir (f a)],mzero)

liftCir3 :: MonadPlus m => (a -> a -> a ->  (m b)) -> Circuit a (m b)
liftCir3 f  = Circuit $ \a -> ([liftCir2 (f a)],mzero)

--This needs testing, It should be that first is first.
liftCirN :: MonadPlus m => Int -> ([a] -> (m b)) -> Circuit a (m b)
liftCirN n f 
	| n <  1 = liftCir $ const mzero
	| n == 1 = Circuit $ \a -> ([],f [a])
	| n >  1 = Circuit $ \a -> ([liftCirN (n-1) (\xs -> f (a : xs) ) ],mzero)


chainCir :: Circuit b c -> Circuit a b -> Circuit a c
chainCir cir2 cir1 =
	Circuit $ \a ->
		let
			(cir1',b) = unCircuit cir1 $ a
			(cir2',c) = unCircuit cir2 $ b
		in (liftA2 chainCir cir2' cir1',c)
--

--This adds a Circuit to the list of returned Circuits
appendCir :: Circuit a b -> Circuit a b -> Circuit a b
appendCir cir2 cir1 = 
	Circuit $ \a -> 
		let (cir1',b) = (unCircuit cir1) $ a 
		in (cir2 : cir1',b)
-- i.e. cir2 `appendCir` cir1 adds cir2 to cir1

--This combines two parsers into one applied simultaniusly, using mplus
combineCir :: MonadPlus m => Circuit a (m b) -> Circuit a (m b) -> Circuit a (m b)
combineCir cir1 cir2 = 
	Circuit $ \a ->
		let 
			(cir1',b1) = (unCircuit cir1) $ a 
			(cir2',b2) = (unCircuit cir2) $ a
		in ( mplus cir1' cir2' , mplus b1 b2 ) --nb mplus == (++) in the second case.

--This combines n parsers into one, generalizing combineCir
combineNCir :: MonadPlus m => [Circuit a (m b)] -> Circuit a (m b)
combineNCir circuits =
	Circuit $ \a ->
		let
			circuits' = msum . (map fst) $ (map unCircuit circuits) <*> (pure a)
			b'        = msum . (map snd) $ (map unCircuit circuits) <*> (pure a)
		in (circuits',b')


applyCircuits circs obj = unCircuit (combineNCir circs) $ obj


instance C.Category Circuit where
	id = liftCir id
	(.)= chainCir




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