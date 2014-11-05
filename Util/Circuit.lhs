\begin{code}


module Util.Circuit where


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
	| n >  1 = Circuit $ \a -> ([liftCirN (n-1) (\xs -> f (a : xs) ) ],mzero)

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
-- i.e. cir2 `appendCir` cir1 adds cir2 into cir1

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

applyCircuits :: MonadPlus m => [Circuit a (m b)] -> a -> ([Circuit a (m b)],m b)
applyCircuits circs obj = unCircuit (combineNCir circs) $ obj


instance C.Category Circuit where
	id = liftCir id
	(.)= chainCir


\end{code}

