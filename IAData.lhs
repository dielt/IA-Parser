
\begin{code}
{-# LANGUAGE ExistentialQuantification  #-}


module IAData where

import Data.Maybe
import Data.List
import Control.Monad.Trans.State
import Control.Monad

import IAUtil
import IADataBase

\end{code}

Some thoughts, I would like to keep all these data definitions seperate from main code
Even in a seperate file, its becoming increasingly messy
Ideally we could have each file at no more than 250 lines
The problem is that there isn't really an obvious way to split this up
If we were to try seperating class and data strictly we run into circular requirements.

The easiest thing to split out is basic data defs.





Object Class definitions and related instances, including hetrogeneous container

\begin{code}

class Object o where
	idn :: o -> Id
	loc :: o -> Coord -- Coord a n => o -> a
	setLoc :: o -> Coord -> o -- Coord a n => o -> a -> o
	names :: o -> [(String,Int)]
	things :: World -> [o]
	setThings :: World -> [o] -> World
	objVolume :: o -> Ml
	parent :: o -> Id --nb, where parent id == world id the object is just in the world
	setParent :: o -> Id -> o


data ObjectA = forall a.  Object a => ObjectA a

--note Cannot derive Eq due to non haskell 98 complience.
instance Eq ObjectA where
	(==) a b = 
		if idn a == idn b 
			then True
			else False

instance Object ObjectA where
	idn (ObjectA o) = idn o
	loc (ObjectA o) = loc o
	setLoc (ObjectA o) = \c -> ObjectA $ setLoc o c
	names (ObjectA o) = names o
	things = worldObjects
	setThings wrld list = foldr (\(ObjectA o) w ->  (worldAObj o w) ) (clearWorldObjects wrld) list
	objVolume (ObjectA o) = objVolume o
	parent (ObjectA o) = parent o
	setParent (ObjectA o) = \p -> ObjectA $ setParent o p

worldObjects :: World -> [ObjectA]
worldObjects wrld = 
	map ObjectA (people wrld) ++ 
	map ObjectA (bottles wrld) ++
	[ObjectA wrld]

\end{code}

This is the basic object managment code, note these delete the objects from the world, not just from inventories

\begin{code}

-- (tail [o]) == [] and allows us to set the type of an empty list to a type we cannot access
clearWorldObjects :: World -> World 
clearWorldObjects wrld = foldr (\(ObjectA o) w -> setThings w (tail [o]) ) wrld (worldObjects wrld)

worldAObj :: Object a => a -> World -> World
worldAObj obj wrld = setThings wrld (obj : (things wrld))

--removes everything with a matching idt
worldDObj :: Id -> World -> World
worldDObj idt wrld = worldFoldFilter wrld test (\obj w -> setThings w (delete obj (things w) ) ) wrld
	where
		test :: ObjectA -> Bool
		test = \o -> idn o == idt 

worldRObj :: Object a => a -> World -> World
worldRObj obj wrld = (worldAObj obj) . (worldDObj (idn obj)) $ wrld 

\end{code}




Class definitions and associated code for Container and FluidContainer

\begin{code}

class Object c => Container c where
	inventory :: c -> [Id]
	setInventory :: c -> [Id] -> c
	checkInventory :: c -> Bool
	checkInventory = \_ -> False --let foldr ()
	capacity :: c -> Ml --capacity of containers
	armSpan :: c -> Mm
	containerNames :: c -> [String] -- length of list is number of containers
	containerNames = \c -> [fst . head $ names c]
	locked  :: c -> Bool

	
class Container c => FluidContainer c where

data ContainerA = forall a.  Container a => ContainerA a

worldContainers :: World -> [ContainerA]
worldContainers wrld = []

instance Eq ContainerA where
	(==) a b = 
		if idn a == idn b 
			then True
			else False

instance Object ContainerA where
	idn (ContainerA o) = idn o
	loc (ContainerA o) = loc o
	setLoc (ContainerA o) = \c -> ContainerA $ setLoc o c
	names (ContainerA o) = names o
	things = worldContainers
	setThings wrld list = foldr (\(ContainerA o) w -> (worldAObj o w) ) (clearWorldObjects wrld) list
	objVolume (ContainerA o) = objVolume o
	parent (ContainerA o) = parent o
	setParent (ContainerA o) = \p -> ContainerA $ setParent o p

instance Container ContainerA where
	inventory (ContainerA o) = inventory o
	setInventory (ContainerA o) = \i -> ContainerA $ setInventory o i
	checkInventory (ContainerA o) = checkInventory o
	capacity (ContainerA o) = capacity o
	armSpan (ContainerA o) = armSpan o
	containerNames (ContainerA o) = containerNames o
	locked (ContainerA o) = locked o

data FluidContainerA = forall a. FluidContainer a => FluidContainerA a

worldFluidContainers :: World -> [FluidContainerA]
worldFluidContainers wrld = []

instance Eq FluidContainerA where
	(==) a b = 
		if idn a == idn b 
			then True
			else False

instance Object FluidContainerA where
	idn (FluidContainerA o) = idn o
	loc (FluidContainerA o) = loc o
	setLoc (FluidContainerA o) = \c -> FluidContainerA $ setLoc o c
	names (FluidContainerA o) = names o
	things = worldFluidContainers
	setThings wrld list = foldr (\(FluidContainerA o) w ->  (worldAObj o w) ) (clearWorldObjects wrld) list
	objVolume (FluidContainerA o) = objVolume o
	parent (FluidContainerA o) = parent o
	setParent (FluidContainerA o) = \p -> FluidContainerA $ setParent o p
	
instance Container FluidContainerA where
	inventory (FluidContainerA o) = inventory o
	setInventory (FluidContainerA o) = \i -> FluidContainerA $ setInventory o i
	checkInventory (FluidContainerA o) = checkInventory o
	capacity (FluidContainerA o) = capacity o
	armSpan (FluidContainerA o) = armSpan o
	containerNames (FluidContainerA o) = containerNames o
	locked (FluidContainerA o) = locked o


\end{code}



And the same for Alive

\begin{code}
class Object a => Alive a where
	intent :: a -> Maybe Intent
	setIntent :: a -> Maybe Intent -> a
	player :: a -> Bool
	
data AliveA = forall a. Alive a => AliveA a

instance Object AliveA where
	idn (AliveA o) = idn o
	loc (AliveA o) = loc o
	setLoc (AliveA o) = \c -> AliveA $ setLoc o c
	names (AliveA o) = names o
	things = worldAlives
	setThings wrld list = foldr (\(AliveA o) w ->  (worldAObj o w) ) (clearWorldObjects wrld) list
	objVolume (AliveA o) = objVolume o
	parent (AliveA o) = parent o
	setParent (AliveA o) = \p -> AliveA $ setParent o p
	
instance Alive AliveA where
	intent (AliveA o) = intent o
	setIntent (AliveA o) = \i -> AliveA $ setIntent o i
	player (AliveA o) = player o

instance Eq AliveA where
	(==) a b = 
		if idn a == idn b 
			then True
			else False

worldAlives :: World -> [AliveA]
worldAlives wrld = map AliveA (people wrld)



\end{code}



The various worldFolds, now as a class

\begin{code}

class Object c => WorldFold c where
	anonObjects :: World -> [c]
	worldFunction :: World -> ([c] -> b ) -> b
	worldFunction = \wrld f -> f . anonObjects $ wrld
	worldFoldFilter :: World -> (c -> Bool) -> (c -> b -> b) -> b -> b
	worldFoldFilter = \wrld test f z  -> foldr f z (filter test (anonObjects wrld))
	worldFold :: World -> (c -> b -> b) -> b -> b
	worldFold = \wrld f z -> worldFoldFilter wrld (\_ -> True) f z
	worldAppId :: World -> (c -> b) -> Id -> Maybe b
	worldAppId = \wrld f idt -> worldFoldFilter wrld (\x -> idn x == idt) (\a b -> Just $ f a) Nothing

instance WorldFold ObjectA where
	anonObjects = worldObjects
	
instance WorldFold ContainerA where
	anonObjects = worldContainers
	
instance WorldFold FluidContainerA where
	anonObjects = worldFluidContainers
	
instance WorldFold AliveA where
	anonObjects = worldAlives
	
\end{code}




Note If we want to make the world a container and object we should make sure this fn has the proper (nonstandard) behaviour.

What does this require,

Well that worldAppIdContainer/worldAppObj etc. contains the world

Also that 
worldRObj obj wrld = obj 
	where obj :: World

Because  worldAObj obj world = setThings wrld (obj : (things wrld)) = setThings wrld [obj]
We just need setThings w ws = head ws 


\begin{code}

--ok so, see above, this should work even where the source or target are the world.
moveItemContainer :: World -> Id -> Id -> Id -> Maybe World
moveItemContainer wrld sourceId targId itmId = 
	(worldAppId wrld (takeFromInv wrld) sourceId ) >>= 
	(\w -> worldAppId w    (addToInv w) targId   ) >>= 
	(\w -> worldAppId w  (moveParent w) itmId    )
		where
			takeFromInv :: World -> ( ContainerA -> World )
			takeFromInv = \w -> (\c -> worldRObj (setInventory c $ delete itmId $ inventory c) w )
			addToInv :: World -> ( ContainerA -> World )
			addToInv = \w -> (\c -> worldRObj (setInventory c $ itmId : (inventory c) ) w )
			moveParent :: World -> ( ObjectA -> World )
			moveParent = \w -> (\o -> worldRObj (setParent o $ targId ) w)


\end{code}




Object Data defs

\begin{code}

data Person = Person 
	{pId :: Id
	,pLoc :: Coord
	,pVolume :: Ml
	,pParent :: Id
	,pIntent :: Maybe Intent
	,pPlayer :: Bool
	}
newPerson :: Id -> Person
newPerson id = Person 
	{pId = id
	,pLoc = Coord (0,0)
	,pVolume = Ml 66400
	,pParent = Id 0
	,pIntent = Nothing
	,pPlayer = False
	}

data Bottle = Bottle 
	{bottleId :: Id
	,bottleLoc :: Coord
	,bottleVolume :: Ml
	,bottleParent :: Id
	}
newBottle :: Id -> Bottle
newBottle id = Bottle 
	{bottleId = id
	,bottleLoc = Coord (0,0)
	,bottleVolume = Ml 341
	,bottleParent = Id 0
	}

type People = [Person]
type Bottles = [Bottle]

\end{code}



Object Instance definitions

\begin{code}

	
instance Object Person where
	idn = pId
	loc = pLoc
	setLoc = \o c -> o{pLoc=c}
	names = \_ -> [("person",0)]
	things = people
	setThings = \w p -> w{people=p}
	objVolume = pVolume
	parent = pParent
	setParent = \o p -> o{pParent=p}
	
instance Alive Person where
	intent = pIntent
	setIntent = \p i -> p{pIntent=i}
	player = pPlayer

	
instance Object Bottle where
	idn = bottleId
	loc = bottleLoc
	setLoc = \o c -> o{bottleLoc=c}
	names = \_ ->  [("bottle",0)]
	things = bottles
	setThings = \w b -> w{bottles=b}
	parent = bottleParent
	setParent = \o p -> o{bottleParent=p}
	objVolume = bottleVolume



\end{code}




World definition

\begin{code}
data World = World {
	seed :: Integer
	,tick :: Integer
	,wrldId :: Id
	,people :: People
	,bottles:: Bottles
	,wrldInv:: [Id]
	}

instance Object World where
	idn = \_ -> Id 0
	loc = \_ -> Coord (10^100,10^100) --Obviously not to be used
	setLoc = \w _ -> w
	names = \_ -> []
	things = \w -> tail [w]
	setThings = \w ws -> fromJust $ (listToMaybe ws) `mplus` (Just w)
	objVolume = \_ -> Ml (10^27) --apparently this is somewhat accurate, order of mag, via wolfram alpha
	parent = \_ -> Id 0
	setParent = \w _ -> w 

--if we create Surface as distinct from Container, world should be Surface
instance Container World where
	inventory = \w -> wrldInv w
	setInventory = \w i -> w{wrldInv=i}
	checkInventory = \_ -> True
	capacity = \_ -> Ml (5*(10^18))
	armSpan = \_ -> Mm 0
	containerNames = \_ -> []
	locked = \_ -> False




\end{code}



\begin{code}

newWorld = World
	{seed = 0
	,tick = 0
	,wrldId = Id 0
	,people = []
	,bottles = []
	,wrldInv = []
	}

incId :: Id -> Id
incId (Id x) = Id (x+1)

incIdW wrld = let i = wrldId wrld in (i,wrld{wrldId = incId i})

incTick :: State World ()
incTick = do --this is a terrible way to do things
	wrld <- get
	let (t,newWrld) = (\w -> ((),w{tick = (tick w) + 1})) wrld 
	put newWrld
	return ()


getId :: State World Id
getId = do
	wrld <- get
	let (i,newWrld) = incIdW wrld
	put newWrld
	return i


getIds = liftM2 (,) getId getId
\end{code}



