\begin{code}
module IALexer where

import Control.Monad

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

So we are going to have problems with using Intent as part of a token, as this includes things like Id
However what this means is we want some sort of partial Intent as a token, i.e. we should be able 

The main problem I have with this is that we either need to remember to always create a tokenized intent
or else transistion to using that token in intent, but that leads us to the question of unneeded information

ideally we'd have something like




\begin{code}


data ActionToken = MoveT | GetT | LookT | SysComT SysIntent deriving (Eq,Show)

data Token = Affirm Bool | Name String | Action ActionToken [Token] | Relation Direction deriving (Eq,Show)

type TokenCollection = [[Token]]

type Lexer = Circuit String (Maybe Token)

formatTokens :: [Token] -> [Token] -- this just returns the arguemts of an action to the normal list.
formatTokens = foldr fn [] --idk if it is even going to be wanted
	where
		fn :: Token -> [Token] -> [Token]
		fn (Action x xs) list = ((Action x []) : xs) ++ list
		fn x list = x : list

yesNoLexer :: Lexer
yesNoLexer = liftCir $ \str ->
	if' (elem str affirmSyn) (Just $ Affirm True) $
	if' (elem str negSyn)    (Just $ Affirm False) $
	Nothing
	
sysEventLexer :: Lexer
sysEventLexer = liftCir $ \str -> msum $
	[ifM (elem str quitSyn) (Just $ Action (SysComT Quit) [] )
	,ifM (elem str helpSyn) (Just $ Action (SysComT Help) [])
	,ifM (elem str settingSyn) (Just $ Action (SysComT Setting) [])
	,ifM (elem str versionSyn) (Just $ Action (SysComT VerNum) [])
	]
	
actionGetLexer :: Lexer
actionGetLexer = combineNCir --lots of repetition here idk what is doable
	[liftCir2 $ \str1 str2 -> ifM (elem str1 getSyn) (Just $ Action GetT [(Name str2)]) --get x 
	,liftCirN 4 $ \(str1:str2:str3:str4:xs) -> ifM ((elem str1 getSyn) && (elem str3 inSyn)) 
		(Just $ Action GetT [(Name str2),(Relation $ Rel In),(Name str3)] ) --get x in y
	,liftCirN 4 $ \(str1:str2:str3:str4:xs) -> ifM ((elem str1 getSyn) && (elem str3 onSyn)) 
		(Just $ Action GetT [(Name str2),(Relation $ Rel On),(Name str3)] ) --get x on y
	,liftCirN 4 $ \(str1:str2:str3:str4:xs) -> ifM ((elem str1 getSyn) && (elem str3 belowSyn)) 
		(Just $ Action GetT [(Name str2),(Relation $ Rel Below),(Name str3)] ) --get x below y
	,liftCirN 5 $ \(str1:str2:str3:str4:str5:xs) -> ifM ((elem str1 getSyn) && (elem str3 inSyn) && (elem str4 inSyn))
		(Just $ Action GetT [(Name str2),(Relation $ Rel In),(Name str3)] ) --get x from in y -> get x in y
	,liftCirN 5 $ \(str1:str2:str3:str4:str5:xs) -> ifM ((elem str1 getSyn) && (elem str3 onSyn) && (elem str4 onSyn))
		(Just $ Action GetT [(Name str2),(Relation $ Rel On),(Name str3)] ) --get x from on y -> get x on y
	,liftCirN 5 $ \(str1:str2:str3:str4:str5:xs) -> ifM ((elem str1 getSyn) && (elem str3 inSyn) && (elem str4 belowSyn)) --first 'inSyn' is intentional
		(Just $ Action GetT [(Name str2),(Relation $ Rel Below),(Name str3)] ) --get x from below y -> get x below y
	]

--I am still concerned about how we interpret various parses, especially to do with this first result
actionMoveLexer :: Lexer
actionMoveLexer = combineNCir 
	[liftCir $ dirLexer >=> (\dir -> Just $ Action MoveT [Relation dir] ) --i.e. is this too presumptive?, should it only be the second
	,liftCirN 2 $ \(str1:str2:xs) -> ifM (elem str1 moveSyn) (dirLexer str2 >>= (\dir -> Just $ Action MoveT [Relation dir] ))
	,liftCirN 3 $ \(str1:str2:str3:xs) -> ifM (elem str1 moveSyn) (dirLexer str2 >>= (\dir -> Just $ Action MoveT [(Relation dir),(Name str3)] ))
	]

dirLexer :: String -> (Maybe Direction) --this doesn't really need to be a circuit, and it was so.
dirLexer = \str -> msum $
	[ifM (elem str inSyn) (Just $ Rel In)
	,ifM (elem str outSyn) (Just $ Rel Out)
	,ifM (elem str onSyn) (Just $ Rel On)
	,ifM (elem str belowSyn) (Just $ Rel Below)
	,ifM (elem str northSyn) (Just $ Abs North)
	,ifM (elem str southSyn) (Just $ Abs South)
	,ifM (elem str eastSyn) (Just $ Abs East)
	,ifM (elem str westSyn) (Just $ Abs West)
	,ifM (elem str downSyn) (Just $ Abs Down)
	,ifM (elem str upSyn) (Just $ Abs Up)
	,ifM (elem str hereSyn) (Just $ Abs Here)
	]
--


\end{code}


I keep thinking that we ought to be able write a general function for every sort of Action
This would require some sort of class of actions,
It would have to at least provide 
synonyms :: a -> [String]
We would then need some place to store all the actions
presumably these would also end up embedded in the world
and then we get an issue of 
ActionToken :: a -> ActionToken
As now all of a suddem ActionToken is a class and we've just moved things back


