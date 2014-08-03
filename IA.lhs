
\begin{code}
module IA where


--import System.Console.ANSI 
import System.IO 


import Data.Char {-
import Data.Maybe
import Control.Applicative
import Control.Monad -}
--import Control.Monad.Trans.State
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












