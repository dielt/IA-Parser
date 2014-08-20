\begin{code}
module IALexer where

import IADataBase
import IASyn
import IAUtil

\end{code}

Ok so this file is going to be the replacement for the first stage of our parser

The idea is it can convert some string of input into collection of possible syntactic meanings

These trails of tokens would then be parsed into a viable command at some later point

The idea is to perserve any ambiguity such that we can later attempt all possible inputs for validity

What this points to is the need for some way to represent all the possible meanings i.e. a general list of tokens

What sort of tokens do we need

we need an

agree/disagree

name string

and some way of tokenizing actions.

the initial solution to actions would be some sort of class. i.e. 

class Intent a where
	doIntent :: World -> Id -> a -> World
	intent :: String/Token

or something, the key here is that we can implicitly enforce any constraint 
based on failure of the worldAppId used with the source id provided

What would be really neat is if we could allow for instances of Intent to be declared in seperate files
Combined prerequisite ability to <partially> define syntax for Tokens and we could essentially allow for
User modifiable Actions, i.e. a simple language which allows for interactions within the world.


Note it is important to keep in mind two things, ideally a token should be the minimum parsable syntax
However it is even more important that we do not allow World or similar to sneak in.


\begin{code}

data Token = Affirm Bool | Name String | Action Intent [Token] | Relation Direction deriving (Eq,Show)

type TokenCollection = [[Token]]

type Lexer = Circuit String (Maybe Token)

yesNoLexer :: Lexer
yesNoLexer = liftCir $ \str ->
	if' (elem str affirmSyn) (Just $ Affirm True) $
	if' (elem str negSyn)    (Just $ Affirm False) $
	Nothing
	
sysEventLexer :: Lexer
sysEventLexer = liftCir $ \str -> 
	if' (elem str quitSyn) (Just $ Action (SysCom Quit) [] ) $
	if' (elem str helpSyn) (Just $ Action (SysCom Help) []) $
	Nothing



\end{code}

