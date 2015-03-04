\begin{code}
{-# LANGUAGE TypeFamilies #-}


module Util.Circuit where

import Util.Tree

import Data.Tree
import Control.Monad
import qualified Control.Category as C
import Control.Applicative



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

--Generalization of the above, first should be first.
liftCirN :: MonadPlus m => Int -> ([a] -> (m b)) -> Circuit a (m b)
liftCirN n f 
	| n <  1 = liftCir $ const mzero
	| n == 1 = Circuit $ \a -> ([],f [a])
	| n >  1 = Circuit $ \a -> ([liftCirN (n - 1) (\xs -> f (a : xs) ) ],mzero)

--lifted (.)
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
-- i.e. cir2 `appendCir` cir1 adds cir2 into cir1, note this occurs regardless of success of cir1 for the input

--lifts appendCir to lists
appendCirs :: Circuit a b -> [Circuit a b] -> Circuit a b
appendCirs cir1 cirs = foldr appendCir cir1 cirs

--This combines two parsers into one applied simultaniusly, using mplus
combineCir :: MonadPlus m => Circuit a (m b) -> Circuit a (m b) -> Circuit a (m b)
combineCir cir1 cir2 = 
	Circuit $ \a ->
		let 
			(cir1',b1) = (unCircuit cir1) $ a 
			(cir2',b2) = (unCircuit cir2) $ a
		in ( mplus cir1' cir2' , mplus b1 b2 ) --nb mplus == (++) in the first case.

--This combines n parsers into one, generalizing combineCir
combineNCir :: MonadPlus m => [Circuit a (m b)] -> Circuit a (m b)
combineNCir circuits =
	Circuit $ \a ->
		let
			circuits' = msum . (map fst) $ (map unCircuit circuits) <*> (pure a)
			b'        = msum . (map snd) $ (map unCircuit circuits) <*> (pure a)
		in (circuits',b')

--applyies a list of circuits to an input, collects the results in two lists
applyCircuitsList :: [Circuit a b] -> a -> ([Circuit a b],[b])
applyCircuitsList cir obj = foldr  (\(cirs,b) (cirs',bs) -> ( cirs ++ cirs' , b : bs) ) ([],[]) $ map unCircuit cir <*> (pure obj)

applyCircuitsM :: MonadPlus m => [Circuit a (m b)] -> a -> ([Circuit a (m b)],m b)
applyCircuitsM circs obj = unCircuit (combineNCir circs) $ obj




instance C.Category Circuit where
	id = liftCir id
	(.)= chainCir


\end{code}


Makes explicit the tree structure of circuits
\begin{code}

treeToCircuit :: Tree (a -> b) -> Circuit a b
treeToCircuit (Node f fs) = appendCirs (liftCir f) (map treeToCircuit fs)

instance TreeAnalogue (Circuit a b) where
	type TreeType (Circuit a b) = (a -> b)
	treeToData = treeToCircuit


listFnToCircuit :: ([a] -> b) -> Circuit a b
listFnToCircuit f = let 
		g xs = Circuit $ \x -> ( [g (x : xs )] , f (x : xs)  )
	in g []

--

--note how this works is that we take running out of circuits as a seperate error then any failure returned by the circuit
--we want to express failure to parse differantly then running out of material to be parsed. 
appCircuitList :: [Circuit a b] -> [a] -> [Tree (Maybe b)]
appCircuitList cirs list = 
	if null list
		then []  -- the differance between null list and cirs, allows us to tell whether we are done parsing or have failed. 
		else if null cirs
			then [Node Nothing []]
			else map (\(cirs',b) -> Node ( Just b ) (appCircuitList cirs' (tail list)) ) $ (map unCircuit cirs) <*> [ head list ]
--}





	{-
	
treeFnToCircuit :: MonadPlus m => (Tree a -> (m b)) -> Circuit a (m b)
treeFnToCircuit f = let
	g xs = Circuit $ \x -> g 
	-}

\end{code}


	forestToData :: Forest (TreeType t) -> t
	dataToForest :: t -> Forest (TreeType t) 






