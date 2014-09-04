\begin{code}


module Util.StateT where


import Data.Maybe
import Control.Monad
import Control.Monad.State.Strict

import Util.Base

\end{code}



All of our various stateT utility code, 
there are probably library equivilents to some of these
\begin{code}

maybeModifyT ::  Monad m => (s -> Maybe s) -> StateT s m ()
maybeModifyT f = do
	s <- get
	let s' = f s
	if isNothing s'
		then return ()
		else put $ fromJust s'

maybeModify :: (s -> Maybe s) -> State s ()
maybeModify f = do
	s <- get
	let s' = f s
	if isNothing s'
		then return ()
		else put $ fromJust s'
	
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



