
\begin{code}


module Text.Format where

import Data.List
import Data.Char

\end{code}

\begin{code}

--not the nicest way of doing things
capSentence = reverse . fst . (foldl (\(l,t) a -> if t && isLetter a then ((toUpper a) : l , not t) else (a:l,t)) ([],True)) . (map toLower)

getExtension = reverse . (takeWhile ('.' /= )) . reverse

checkExtension :: String -> String -> Bool
checkExtension ext = (ext ==) . getExtension


\end{code}