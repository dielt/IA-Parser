
\begin{code}
module IA where


--import System.Console.ANSI 
import System.IO 


import Data.Char {-
import Data.Maybe
import Control.Applicative -}
import Control.Monad
import Control.Monad.Trans.State
--import Graphics.UI.Gtk
--import System.Environment
--import Graphics.UI.SDL as SDL

\end{code}


Local modules

\begin{code}

import IAData
import IAParse
import IAPath
import IAUtil
import IASyn
import IADataBase

\end{code}





Some basic functions rescued

\begin{code}

getInput :: IO [String]
getInput = --hFlush ensures that we print the prompt before getLine
	putStr "\n>" >> hFlush stdout >>	getLine >>= 
	(\x -> let y = (filter (not . flip elem fillerWords ) ) . words . (map toLower) $ x in 
		if null y 
			then getInput 
			else return y
	)

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
	
  
--forces a function to act per line on streaming input
eachLine :: (String -> String) -> (String -> String)
eachLine f = unlines . map f . lines

\end{code}



We are having problem wherein we need both IO and State at the same time, 
There are obvious solutions, but I think they just allow us to avoid restructuring our program
Essentially the issue arises from yesNo, and storing sysEvents in the world
I think a good solution is to checkSys immediatly after parsing
However there is still the problem of the world requirement in iaParse

It might be advantageous to seperate syntactic parsing, from a second validation stage.

The idea being something like this, where we can recurse on this function until we have a non-empty Intent

parseSyntax :: Object a => a -> String -> [Intent]







Ahah, here is where State World comes in handy, though the current structure does not make use of it
We can see how this allows for a more complicated and immediatly explicit strucuture than the previous linear loop of IO



\begin{code}

gameLoop :: State World (IO ())
gameLoop = do
	get >>= put . doAction
	wrld <- get
	get >>= put . clearSysEvent
	if sysEvent wrld == Just Quit
		then return $ return ()
		else gameLoop

doAction :: World -> World
doAction = id

getPlayerIntent :: Alive a => a -> State World (IO (Maybe Intent))
getPlayerIntent peep = get >>= return . fn
	where
		fn wrld = 
			(getInput >>= \input -> return $ parseIntentCombination wrld peep input) >>= \intnt -> 
				if intnt == Nothing 
					then fn wrld 
					else if intnt == Just (SysCom Quit)
						then putStr "\nAre you sure you would like to quit?" >> yesNo >>= \response -> if response
							then return (Just $ SysCom Quit)
							else fn wrld
						else return intnt
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












