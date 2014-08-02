
\begin{code}
module IA where


--import System.Console.ANSI 
import System.IO 

{-
import Data.Char
import Data.Maybe
import Control.Applicative
import Control.Monad -}
import Control.Monad.Trans.State
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


\begin{code}

main :: IO ()
main = return ()

--forces our function to act per line
eachLine :: (String -> String) -> (String -> String)
eachLine f = unlines . map f . lines

\end{code}




