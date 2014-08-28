
\begin{code}
module IA where


--import System.Console.ANSI 
import System.IO 


import Data.Char
import Data.Maybe
import Data.Tree
import Data.List
--import Control.Applicative -}
import Control.Monad
import Control.Monad.Trans.State
--import Graphics.UI.Gtk
--import System.Environment
--import Graphics.UI.SDL as SDL

\end{code}


Local modules

\begin{code}

import IAData
import IAParse2
import IALexer
import IAPath
import IAUtil
import IASyn
import IADataBase

\end{code}



\begin{code}

--we may want to transistion towards even this using our tokens.
yesNo :: IO Bool
yesNo = do
	input <- getInput
	if null input 
		then yesNo 
		else if elem (head input) affirmSyn
			then return True
			else if elem (head input) negSyn
				then return False
				else putStr "Please input either yes or no." >> yesNo



getInput :: IO [String]
getInput = --hFlush ensures that we print the prompt before getLine
	putStr "\n>" >> hFlush stdout >>	getLine >>= 
	(\x -> let y = (filter (not . flip elem fillerWords ) ) . words . (map toLower) $ x in 
		if null y 
			then getInput 
			else return y
	)

\end{code}



We are having problem wherein we need both IO and State at the same time, 
There are obvious solutions, but I think they just allow us to avoid restructuring our program
Essentially the issue arises from yesNo, and storing sysEvents in the world
I think a good solution is to checkSys immediatly after parsing
However there is still the problem of the world requirement in iaParse

It might be advantageous to seperate syntactic parsing, from a second validation stage.

The idea being something like this, where we can recurse on this function until we have a non-empty Intent

parseSyntax :: Object a => a -> String -> [Intent]



\begin{code}

main :: IO ()
main = evalStateT prepWorld newWorld

prepWorld :: StateT World IO ()
prepWorld = do
	stateToStateT addPlayerWorld
	gameLoop
	
\end{code}





Ahah, here is where State World comes in handy, though the current structure does not make use of it
We can see how this allows for a more complicated and immediatly explicit strucuture than the previous linear loop of IO

Actually I still am not convinced as to the utility of State here. We are still doing an awful lot of fiddling

Of course this does allow us to more cleanly implement that sort of system check, without removing our ability to have a continious flow of changes.

We have a problem however, of how we can have io affect the state it is within

i.e. can we have a function IO World -> State World (IO())
modify :: MonadState s m => (s -> s) -> m ()

problem solved: stateTMerger/stateTMergerJoin

Really the predominant advantage of State thus far, has been to force us to be overally strict about code structure.
Which is of doubtful overall benefit, given the hoops it has posed to elicite that


\begin{code}

gameLoop :: StateT World IO ()
gameLoop = do
	doActions
	wrld <- get
	modify clearSysEvent
	modify ( \w -> w {tick = (tick w)+1} )
	case sysEvent wrld of
		Just Quit -> return ()
		Just Help -> do
			stateTMonadLift $ putStrLn "Help: TODO"  
			gameLoop
		Just VerNum -> do
			stateTMonadLift $ putStrLn "VersionNumber:0 --Update this."  
			gameLoop
		_ -> gameLoop

doActions :: StateT World IO ()
doActions = do
	get >>= ( \w -> worldFoldFilterStateT w player doActionPlayer )
	get >>= ( \w -> worldFoldFilterStateT w (not . player) doActionAI )


doActionPlayer :: AliveA -> StateT World IO ()
doActionPlayer peep = do
	toks <- stateTPlayerInput
	wrld <- get --ugly
	let wrld' = foldr mplus Nothing $ map (doAction peep wrld) (tokensToIntent toks)
	if isNothing wrld'
		then doActionPlayer peep
		else put $ fromJust wrld'

doActionAI :: AliveA -> StateT World IO ()
doActionAI peep =
	return ()


--we are going to want to combine this with verifyIntent(IAParser2) somehow.
--also note, we don't use our new conception of intents being executed by the world here.
doAction :: Alive a => a -> World -> Intent -> Maybe World
doAction peep wrld intnt =
	case intnt of 
		SysCom x -> Just $ wrld{sysEvent=Just x}
		Move x   -> doMove wrld peep x
		_        -> Nothing 

doMove :: Alive a => World -> a -> Target -> Maybe World
doMove wrld peep (Tar Nothing tarId) = return wrld
doMove wrld peep (Tar (Just tarDir) tarId) = return wrld

stateTPlayerInput :: StateT World IO TokenCollection
stateTPlayerInput = stateTMonadLift parsePlayerInput  -- peep = get >>= \wrld -> stateTJoin . return $ parsePlayerInput peep wrld


parsePlayerInput :: IO TokenCollection
parsePlayerInput = 
	getToks >>= parseQuit >>= \toks -> 
		case toks of
			Nothing   -> parsePlayerInput
			otherwise -> return $ fromJust toks

-- we could probably do this in gameLoop now, but I prefer this location. 
--it feels like it makes less assumptions about there only being one player
parseQuit :: TokenCollection -> IO (Maybe TokenCollection)
parseQuit toks = if or $ map checkQuitToken toks
	then putStr "\nAre you sure you would like to quit?" >> yesNo >>= \response -> if response
		then return $ Just toks
		else return $ Nothing --note we return nothing when we need a new tokenCollection
	else return $ Just toks

checkQuitToken :: Tree Token -> Bool
checkQuitToken (Node x []) = x == Action (SysComT Quit) []
checkQuitToken x = False

getToks :: IO TokenCollection
getToks = getInput >>= return buildLexForest

yesNo' ::
yesNo' =  

\end{code}



This thing is problematic, it would be better to catch sysEvents on syntactic parsing of input

\begin{code}
{-
--idk if this is the best way to do world as a state
--or really if we ought to use state
--also note True == continue False == Stop
checkSys :: State World (IO Bool)
checkSys = do
	wrld <- get
	clearSysEvent
	return $
		case sysEvent wrld of
			Just Quit -> putStr "\nAre you sure you would like to quit?" >> yesNo
			Just Help -> putStr "Todo Help" >> return False
			Nothing   -> return False
-}



\end{code}





There seems to be a tension between strictness in input requests and harshness of punishment for invalid input.
Though it would be fairly easy, given our advances parser capability, to ensure syntactic validity.
The problem arises with impossible arguements, I do not think we can fully eleminate them, due to order priority issues
This means it is possible for a person simply fail to perform an action
I am still not sure if this would be acceptable, or would it be preferable to simply recurse with new input.


There is another problem which arises from our treating Container and Alive as orthogonal classes.
What do we do if a non Container tries to grab something
This is mainly an issue because of our inability to check membership of constraints on the fly, afaik.

The obvious solution to this is just to make Container => Alive and then any living non-container can simply have zero capacity

The next question would be, how does fluid container work for this?
We run into a similar problem with non linear chain.

Ideally we would have some kind of fold combinator, Actually it wouldn't be that hard to do, We shall go experiment












