\begin{code}
module IALexer where

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
	doIntent :: Alive b => World -> b -> a -> World
	intent :: Token

Though there is a problem wherein we may want to enforce ceratain additional constraints.

i.e. 

class Intent a c | a -> c where
	doIntent (Alive b, c b) => World -> b -> a -> World
	intent :: Token

or really the problem here is going to be in worldFold

