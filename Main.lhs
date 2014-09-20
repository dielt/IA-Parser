
\begin{code}
module Main where


--import System.Console.ANSI 
import System.IO 


import Data.Char
import Data.Maybe
import Data.Tree
import Data.List
--import Control.Applicative -}
import Control.Monad
import Control.Monad.State.Strict
--import Graphics.UI.Gtk
--import System.Environment
--import Graphics.UI.SDL as SDL

\end{code}


Local modules

\begin{code}

import Data.ObjectClass
import Data.Base
import IAParse2
import IALexer
import IAPath
import Util.Base
import Util.StateT
import Util.Tree
import IASyn

\end{code}



\begin{code}

--we may want to transistion towards even this using our tokens.
yesNoOld :: IO Bool
yesNoOld = do
	input <- getInput
	if null input 
		then yesNo 
		else if elem (head input) affirmSyn
			then return True
			else if elem (head input) negSyn
				then return False
				else putStr "Please input either yes or no." >> yesNo


--we no longer need the filler words filter, due to tokens
getInput :: IO [String]
getInput = --hFlush ensures that we print the prompt before getLine
	putStr "\n>" >> hFlush stdout >>	getLine >>= 
	(\x -> let y =  words . (map toLower) $ x in 
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
	stateTMonadLift $ putStrLn "loop"
	verifyIntents
	deployIntent
	wrld <- get
	(stateTMonadLift $ putStrLn $ (++)  "Tick:" $ show $ tick wrld )
	stateTMonadLift $ putStrLn $ show $ wrldIntents wrld
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


deployIntent :: StateT World IO ()
deployIntent = do 
	wrld <- get
	foldr (\(idt,tic,intnt) s ->  get >>= \w -> if isNothing intnt then updateIntent idt else
		( case checkIntentTime w idt tic (fromJust intnt) of
			Nothing    -> return () --do nothing
			Just True  -> do
				w <- get
				let (w',str) = doAction idt w (fromJust intnt)
				(if isNothing w' then return () else put $ fromJust w')
				(if null str then return () else stateTMonadLift . putStrLn $ str)
			Just False -> updateIntent idt
		) >> s) (return ()) ( wrldIntents wrld )

\end{code}


so VerifyIntents folds over every living thing and ensures there is a coorsponding actioncollection
and in the second step folds over every actioncollection and checks for a coorsponding living thing.
The second step is probably superfluous
Actually we might want a third step to ensure no duplicate ids, done

\begin{code}

verifyIntents :: StateT World IO ()
verifyIntents = do
	modify 
		(\wrld -> worldFold wrld 
			((\p w -> 
				if elem (idn p) (map fst3 $ wrldIntents w) 
					then w 
					else (w{wrldIntents = (idn p,(-1),Nothing) : (wrldIntents w) })  --note due to -1 tick, this should never trigger, we may wish to transistion to using Maybe Intent however, to make this clearer.
				):: AliveA -> World -> World ) wrld
		)
	modify 
		(\wrld -> wrld { wrldIntents =  foldr 
				(\tok@(idt,_,_) list -> if (isNothing $ worldAppId wrld (idn :: AliveA -> Id) idt ) || (elem idt (map fst3 list))
					then list
					else tok : list
				) [] (wrldIntents wrld)
			}
		)


--note fromJust . worldAppId is justified as this will only be called after checkIntent which already depends on worldAppId
--for now however we don't have any variance in speeds, so idt is currently unused
checkIntentTime :: World -> Id -> Integer -> Intent -> Maybe Bool
checkIntentTime wrld idt tic intnt = let curTime = (tick wrld) - 2 in
	case intnt of
		SysCom _ -> Just $ tic > curTime
		Move _   ->
			if (tic + 11) > curTime
				then if 1 == ( rem (abs $ curTime - tic) 3 )
					then Just True
					else Nothing
				else Just False
		Get _    -> Just $ tic > curTime
		Look _   -> Just $ tic > curTime

updateIntent :: Id -> StateT World IO ()
updateIntent idt = do
	wrld <- get
	let desc = worldAppId wrld describeEnv idt
	if isNothing desc
		then stateTMonadLift $ putStrLn $ "updateIntent, descEnv Id" ++ show idt ++ " not found"
		else fromJust desc
	toks <- stateTPlayerInput
	let intnt = pickIntent idt wrld toks
	modify (\w -> w {wrldIntents = filter (\x -> fst3 x /= idt) (wrldIntents w) } )
	if isNothing intnt
		then updateIntent idt
		else modify (\w -> w {wrldIntents = (idt,tick w,intnt) : (wrldIntents w) } )

updateIntents :: StateT World IO ()
updateIntents = do
	modify (\w -> w{wrldIntents=[]})
	get >>= ( \w -> worldFoldFilterStateT w player updateIntentPlayer )
	get >>= ( \w -> worldFoldFilterStateT w (not . player) updateIntentAI )

updateIntentPlayer :: AliveA -> StateT World IO ()
updateIntentPlayer peep = do
	describeEnv peep
	toks <- stateTPlayerInput
	modify (\w -> let idt = idn peep in w {wrldIntents = (idt , tick w , pickIntent idt w toks) : (wrldIntents w) })
	
	
describeEnv :: AliveA -> StateT World IO ()
describeEnv peep = do
	stateTMonadLift $ putStrLn "DescribeEnv:"
	stateTMonadLift $ putStrLn $ show $ idn peep
	stateTMonadLift $ putStrLn $ show $ loc peep
	

updateIntentAI :: AliveA -> StateT World IO ()
updateIntentAI peep = 
	stateTMonadLift $ putStrLn $ (show $ idn peep) ++ "UpdateIntentAI TODO"

doActions :: StateT World IO ()
doActions = do
	wrld <- get
	foldr (\(idt,tic,intnt) s -> if isNothing intnt then return () else
		( do
			w <- get
			let (w',str) = doAction idt w $ fromJust intnt
			(if isNothing w' then return () else put $ fromJust w')
			(if null str then return () else stateTMonadLift . putStrLn $ str)
		) >> s) (return ()) (wrldIntents wrld)


--we may want some smarter way of sorting multiple viable intents
--i.e. always prefering systemIntents or similar rules
--this would just be a more complicated listToMaybe, type wise at least.
pickIntent :: Id -> World -> TokenCollection -> Maybe Intent
pickIntent idt wrld toks =  
		listToMaybe . filter (checkIntent idt wrld) =<<	worldAppId wrld (\peep -> tokensToIntent wrld peep toks) idt

checkIntent :: Id -> World -> Intent -> Bool
checkIntent = not . isNothing . fst .:: doAction

doAction :: Id -> World -> Intent -> (Maybe World,String)
doAction idt wrld intnt = 
	case intnt of
		SysCom x -> (Just $ wrld{sysEvent=Just x},[])
		Move (Target dir tId) -> ((worldAppId wrld (loc :: ObjectA -> Coord) tId ) >>= \x -> doMove idt x dir wrld , [] )
		Get x -> ( doGet wrld idt x , [] )
		Look (Target dir tId) -> (Nothing,doLook wrld dir idt tId)

\end{code}


\begin{code}

doMove :: Id -> Coord -> Maybe Direction -> World -> Maybe World
doMove idt targ dir wrld =
	let
		getPath x y = lpath manAdj eucDistSqrd x y 10
		g :: Coord -> AliveA -> Maybe World
		g x peep = (join $ liftM listToMaybe $ getPath (loc peep) x) >>= \loc' ->
			Just $ worldRObj (setLoc peep loc') wrld
		f y = join $ worldAppId wrld (g y) idt 
	in
		case dir of
			Nothing -> f targ
			Just (Abs x) -> (coordDir x targ) >>= f
			otherwise -> Nothing

doGet :: World -> Id -> Id -> Maybe World
doGet wrld idt itmId = 
	let
		peeploc = worldAppId wrld (loc :: ContainerA -> Coord) idt
		tarloc = worldAppId wrld (loc :: ObjectA -> Coord) itmId
		getPath x y = lpath manAdj eucDistSqrd x y 10
		targId = fromJust $ (worldAppId wrld (parent :: ObjectA -> Id) itmId) `mplus` (Just $ Id 0)
	in do
		peeploc' <- peeploc
		tarloc' <- tarloc
		(getPath peeploc' tarloc') >>= \_ -> moveItemContainer wrld idt targId itmId

doLook :: World -> Maybe Direction -> Id -> Id -> String
doLook wrld mayDir sourceId tarId =
	case mayDir of
		Nothing -> descObj wrld sourceId tarId
		Just (Abs x) -> descArea wrld 
		Just (Rel x) -> descRel

descObj :: World -> Id -> Id -> String
descObj wrld sourceId targId = worldAppId wrld (("descObj:" ++) show . idn) targId

descAbs :: World -> AbsDirection -> Id -> Id -> String
descArea wrld dir sourceId targId = []

descRel :: World -> RelDirection -> Id -> Id -> String
descRel wrld dir sourceId targId = []

\end{code}





\begin{code}


stateTPlayerInput :: StateT World IO TokenCollection
stateTPlayerInput = stateTMonadLift parsePlayerInput -- peep = get >>= \wrld -> stateTJoin . return $ parsePlayerInput peep wrld


parsePlayerInput :: IO TokenCollection
parsePlayerInput =
	getToks >>= parseQuit >>= \toks ->
		if isNothing toks
			then parsePlayerInput
			else return $ fromJust toks


-- we could probably do this in gameLoop now, but I prefer this location.
--it feels like it makes less assumptions about there only being one player
parseQuit :: TokenCollection -> IO (Maybe TokenCollection)
parseQuit toks = if or $ map checkQuitToken toks
	then putStr "\nAre you sure you would like to quit?" >> yesNo >>= \response -> if response
		then return $ Just toks
		else return $ Nothing --note we return nothing when we need a new tokenCollection
	else return $ Just toks


getToks :: IO TokenCollection
getToks = getInput >>= return . buildLexForest

--not that useful
tokenCheck :: [Token] -> IO Bool
tokenCheck toks = liftM (map treeToList) getToks >>= return . or . map (elem toks)


yesNo :: IO Bool
yesNo = getToks >>= (return . foldr (\tok may -> 
	if isNothing may
		then
			case tok of 
				Node (Affirm x) [] -> Just x
				otherwise -> Nothing
		else may
	) Nothing) >>= (\x -> 
		if isNothing x 
			then (putStr "Please input either yes or no.") >> yesNo 
			else return $ fromJust x
		)
	

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












