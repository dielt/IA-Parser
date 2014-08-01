
\begin{code}
module IA where


--import System.Console.ANSI --this would only be used for text bolding
import System.IO
import Data.Char
import Data.Maybe
import Control.Applicative
import Control.Monad

\end{code}


\begin{code}

import IAData
import Parse
import Path
import Util
import IASyn
import IADataBase

\end{code}


\begin{code}
main :: IO()
main = do
	--setTitle "IA" 
	disclaimer
	--gameLoop $ newPlayer $ newWorld

disclaimer = putStr ""

gameLoop :: World -> IO ()
gameLoop = checkSys <=< descEnvPlayers <=< doActions

\end{code}


Basic input and printing code

\begin{code}
boldPrint text = do
 -- setSGR [SetColor Foreground Vivid White ]
  putStr text
 -- setSGR [Reset]

getInput :: IO [String]
getInput = --hFlush ensures that we print the prompt before getLine
	putStr "\n>" >> hFlush stdout >>	getLine >>= 
	(\x -> let y = (filter (not . flip elem fillerWords ) ) . words . (map toLower) $ x in 
		if null y 
			then getInput 
			else return y
	)
\end{code}


\begin{code}

checkSys :: World ->  IO ()
checkSys wrld = return () {-
	case sysEvent wrld of
		Just Quit -> putStr "\nAre you sure you would like to quit?" >> yesNo >>= flip unless (gameLoop wrld {sysEvent = Nothing})
		Just Help -> doHelp >> (gameLoop $ wrld {sysEvent = Nothing})
		Nothing   -> 
			if worldFold wrld player 
				then gameLoop wrld 
				else  putStrLn "Error, no living player " >> return () -}

{-

playerCheck :: Alive -> Bool -> Bool
playerCheck peeps bool = 
	if bool
		then True
		else foldr (\x y -> if y then True else player x ) False peeps

-}

doHelp = putStrLn "TODO: HELP" 


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


\end{code}
I'm pretty pleased with everything above here, 

\begin{code}
descEnvPlayers :: World -> IO World
descEnvPlayers wrld =
	worldFunction wrld (\peeps blank -> sequence_ . (map (descEnv wrld)) $ peeps ) ( return () ) >> ( return wrld )
	

descEnv :: Alive a => World -> a -> IO ()
descEnv wrld peep = 
	when (player peep) (putStrLn  $ "descEnv:\nPlayerCoordinates: " ++ (show $ loc peep))

--World -> (forall a. Alive a => [a] -> b -> b) -> b -> b
--World -> (forall a. Alive a => [a] -> IO () -> IO () ) -> IO () -> IO () 





--nb We actually don't need to checkInput as a seperate step, it can be done within the doAction stage
{- 
checkInput :: World -> IO World
checkInput wrld = worldFoldAlive wrld getIntents (return wrld)


getIntents :: Alive a => [a] -> IO World -> IO World
getIntents peeps iWorld = do
	wrld <- iWorld
	newPeeps <- mapM ( getIntent wrld ) peeps
	return $ repThingsW wrld newPeeps
-}

getIntent :: Alive a => World -> a -> IO a
getIntent wrld peep =
	if player peep
		then getInput >>= ( (checkIntent wrld) . (setIntent peep) . (parseIntent wrld peep) )
		else doAi peep

checkIntent :: Alive a => World -> a -> IO a
checkIntent wrld peep = 
	if intent peep == Nothing
		then getIntent wrld peep
		else return peep

doAi peep = putStr "TODO: AI" >>  return peep
\end{code}


\begin{code}
doActions :: World -> IO World
doActions wrld = worldFunction wrld (doActs (return wrld))

doActs :: IO World -> [AliveA] -> IO World
doActs iWorld peeps = iWorld >>= (\wrld -> foldM doAct wrld peeps	)

doAct :: World -> AliveA -> IO World
doAct wrld peep = putStrLn ("Player Intent:" ++ (show $ intent peep)) >>
	case (intent peep) of
		Just (SysCom x)            -> return wrld -- return $ worldRObj (wrld {sysEvent = Just x}) (setIntent peep Nothing)
		Just (Move( Tar x y )) -> doMovePath wrld peep y x 10 --  ( lpath manAdj eucDistSqrd (loc peep) y 30 ) x
		Just (Move( Tar x y ))   -> doMoveId wrld peep x y
		Just (Look( Tar x y )) -> (putStrLn $ doLook wrld y x) >> ( return $ worldRObj wrld $ setIntent peep Nothing )
		Just (Look( Tar x y ))   -> (putStrLn $ doEx wrld peep x y) >>  ( return $ worldRObj wrld $ setIntent peep Nothing )
		Just (Get x)               -> doGet wrld peep x >>= \a -> ( return $ worldRObj a $ setIntent peep Nothing ) 
		_                          -> ( getIntent wrld peep ) >>= ( doAct wrld )

doMovePath ::Alive a => World -> a -> Coord -> Maybe Direction -> Int -> IO World
doMovePath wrld peep tarCoor dir roomSize = 
	let targ = ( fromJust $ (dir >>= pickCoord wrld tarCoor) `mplus` Just tarCoor )  in 
	let pth = lpath manAdj eucDistSqrd (loc peep) targ (fromIntegral roomSize^2) in
		if null pth
			then doAct wrld (setIntent peep Nothing) 
			else return wrld -- return $ worldRObj wrld $ setLoc (setIntent peep $ Just $ Move $ Tar Nothing targ) (head pth)

doMoveId :: World -> AliveA -> Maybe Direction -> Id -> IO World
doMoveId wrld peep rel idt = let mayLoc = worldAppId wrld loc idt in
	if mayLoc /= Nothing 
		then if or [rel == Nothing , fromJust mayLoc /= loc peep]
			then doMovePath wrld peep (fromJust mayLoc) Nothing 25
			else case rel of
				Just (Rel In   ) -> return wrld
				Just (Rel Out  ) -> return wrld
				Just (Rel On   ) -> return wrld
				Just (Rel Below) -> return wrld
		else return wrld -- doAct ( worldRObj wrld $ setIntent peep Nothing ) peep


--We should change this to be more agnostic towards the human player.
--The reasoning behind this is how I want to do mirrors. -- see Ig
doLook :: Object a => World -> a -> Maybe Direction -> String
doLook wrld source dir
	| dir == Nothing = doLook wrld source (Just $ Abs Here)
	| dir == Just (Abs Here) = "You see " -- ++ formatNames ( collectObjectNamesRoom wrld ( coordAdd (loc source) (pickDirUnit (fromJust dir) ) ) ) ++ " here."
	| dir == Just (Abs Down) = "You look down and see yourself"
	-- | isDirCardinal (fromJust dir) = "You look " ++ (((map toLower) . show $ fromJust dir)) ++ " and see " ++ formatNames ( collectObjectNamesRoom wrld ( coordAdd (loc source) (pickDirUnit (fromJust dir) ) ) ) ++ "."
	| otherwise = "ERROR: You look " ++ ((map toLower) . show $ fromJust dir) ++ " direction not yet written"

doEx :: Object a => World -> a -> Maybe RelDirection -> Id -> String
doEx wrld peep rel idt = "doEx TODO" {-
	if namePair == Nothing
		then "Error, ID:" ++ show idt ++ "not found"
		else "You look at " ++ properNoun ++ fst ( fromJust namePair )
	where
		namePair = worldAppObj wrld idt foldFn1 
		foldFn1 x = foldl foldFn2 ("Error-NoName:" ++ show idt,-1000) (names x)
		foldFn2 (name1,val1) (name2, val2) = 
			if val1 < val2
				then (name2,val2)
				else (name1,val1)
		properNoun = if snd ( fromJust  namePair ) < 100 then "a " else ""
-}



--we want to be more sensible in picking though maybe that could be done in doMove
pickCoord :: World -> Coord -> Direction -> Maybe Coord
pickCoord wrld coor dir = Just coor {-
	case dir of
		North -> Just $ coordAdd coor northUnit
		South -> Just $ coordAdd coor southUnit
		East  -> Just $ coordAdd coor eastUnit
		West  -> Just $ coordAdd coor westUnit
		_     -> Nothing -}

--right now we don't need IO and should really use Maybe or Either or something.
doGet :: (Alive a,Container a) => World -> a -> Id -> IO World
doGet wrld peep idt = return wrld {- $
	if nearThing wrld (loc peep) 10 idt
		then let x = join $ worldAppObj wrld idt parentObj in
			if x == Nothing
				then worldMoveObj wrld wrld peep idt --wrldRObj wrld (setInv peep (idn : inventory peep))
				else if (let b = worldAppContainer wrld (fromJust x) locked in if b == Nothing then False else fromJust b )
					then wrld
					else fromJust $ worldAppContainer wrld  (fromJust x) (\source -> worldMoveObj wrld source peep idt)
		else wrld -}

\end{code}

