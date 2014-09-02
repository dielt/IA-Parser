\begin{code}
module IAParse2 where

import Data.Tree
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Maybe


import Util.Base
import Util.Circuit
import Util.Tree
import IALexer
import Data.Base
import Data.ObjectClass
import IASyn

\end{code}


\begin{code}

--returns all objects in the world matching the name
--or all of whatever datatype is asked for.
findName ::Object c => World -> String -> [c]
findName wrld str = worldFoldFilter wrld ((elem str) . (map fst) . names) (:) []

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


--there is a problem in here
tokensToIntent :: World -> AliveA -> TokenCollection -> [Intent]
tokensToIntent wrld peep tokens = catMaybes $ mapFullForestM (buildParsedList $ allParsers wrld peep) tokens

--this is going to be useful in a variety of places, and should be shared
verifyIntent :: World -> Id -> Intent -> Maybe Intent
verifyIntent wrld id intnt = 
	case intnt of
		SysCom _ -> Just $ intnt
		_        -> Nothing

allParsers :: World -> AliveA -> [Parser]
allParsers wrld peep = 
	[parseSys
	,parseMove wrld peep
	]
	
--for testing 
quitTree :: Tree Token
quitTree = Node {rootLabel=Action $ SysComT Quit,subForest=[]}

quitList :: [Token]
quitList = [Action $ SysComT Quit]


\end{code}

So right now we rely on the order of parsers to heirarchize parses per path through the token tree
However as I do not think we have any sensible way of perfering a given path
I think it best to simply return a list of whatever the each path returned



\begin{code}

checkQuitToken :: Tree Token -> Bool
checkQuitToken tree = (mapFullTreeM (buildParsedList [parseSys]) tree ) == (Just (SysCom Quit))

--wow, way too much pattern matching going on here
--I don't really know how to avoid it though, given our current structure.
parseSys :: Parser
parseSys = combineNCir
	[
	liftCir2 $ 
		let
			f (Action (SysComT x)) (Name y) = ifM (elem y selfSyn) (Just $ SysCom x)
			f _ _ = Nothing
		in f
	,
	liftCir $ 
		let
			f (Action (SysComT x)) = Just $ SysCom x
			f _ = Nothing
		in f
	]
	
parseMove :: World -> AliveA -> Parser
parseMove wrld peep = combineNCir
	[
	liftCir $
		let 
			f (DirT x) = Just $ Move $ Target (Just x) (idn peep)
			f _ = Nothing
		in f
	,
	liftCir2 $
		let
			f (Action MoveT) (DirT x) = Just $ Move $ Target (Just x) (idn peep)
			f _ _ = Nothing
		in f
	]

--Move (Target (Maybe Direction) Id)


{-
this causes issues
	liftCir2 $ \inp1 (Name y) -> case inp1 of
		(Action (SysComT Help) []) -> ifM (elem y selfSyn) (Just $ SysCom Help)

-}


\end{code}