
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

Some basic util fns
\begin{code}

emptyList :: a -> [a]
emptyList a = tail [a]


\end{code}


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
	setThings wrld list = let clearW = foldr (\(ObjectA o) w -> setThings w $ emptyList o ) wrld (things wrld) in  foldr (\(ObjectA o) w ->  (worldAObj o w) ) clearW list
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
	setThings wrld list = let clearW = foldr (\(ContainerA o) w -> setThings w $ emptyList o ) wrld (things wrld) in  foldr (\(ContainerA o) w ->  (worldAObj o w) ) clearW list
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
	setThings wrld list = let clearW = foldr (\(FluidContainerA o) w -> setThings w $ emptyList o ) wrld (things wrld) in  foldr (\(FluidContainerA o) w ->  (worldAObj o w) ) clearW list
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
	setThings wrld list = let clearW = foldr (\(AliveA o) w -> setThings w $ emptyList o ) wrld (things wrld) in  foldr (\(AliveA o) w ->  (worldAObj o w) ) clearW list
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

Trying to seperate worldFold and AnonObjects as seperate classes I've realized
things == things, though only defined for anonymous objects, 
And given that it is currently a subclass of object the whole thing is uneeded
Especially as we might like to fold over just normal datatype at some point.
Though it does give us the idea of splitting things out of 

\begin{code}

worldFunction :: Object c => World -> ([c] -> b ) -> b
worldFunction = \wrld f -> f . things $ wrld
worldFoldFilter ::Object c => World -> (c -> Bool) -> (c -> b -> b) -> b -> b
worldFoldFilter = \wrld test f z  -> foldr f z (filter test (things wrld))
worldFold :: Object c => World -> (c -> b -> b) -> b -> b
worldFold = \wrld f z -> worldFoldFilter wrld (\_ -> True) f z
worldAppId :: Object c => World -> (c -> b) -> Id -> Maybe b
worldAppId = \wrld f idt -> worldFoldFilter wrld (\x -> idn x == idt) (\a b -> Just $ f a) Nothing

	
\end{code}



Now our attempt at combining worldFold classes

The idea is that we can constuct some new data combinator
And use the intersection of the two datatypes's anon lists as the new list.

\begin{code}

intersectId :: (Object a,Object b) =>  [a] -> [b] -> [(a,b)]
intersectId list1 list2 = let idnList = (map idn list1) `intersect` (map idn list2) in
	foldr 
		(\x xs -> 
			(head . filter (\y -> idn y == x) $ list1 
			,head . filter (\y -> idn y == x) $ list2  
			) : xs 
		) [] idnList



\end{code}
i.e. every object of this list can be considered a member of both previous data types, 
e.g. for A B

instance A (a,b)
	F_A :: A -> C
	F_A (a,b) = F_A a
	
etc


For example

\begin{code}

data AliveContainerA = forall a b. (Container b, Alive a) => AliveContainerA (a,b)

instance Object AliveContainerA where --keep in mind both a and b are the same object, in differant wrappers
	idn (AliveContainerA (a,b)) = idn a --this is going to lead to difficulties in setThings, where we are going to need to recombine the two.
	loc (AliveContainerA (a,b)) = loc a
	setLoc (AliveContainerA (a,b)) = \c -> AliveContainerA $ (setLoc a c,setLoc b c)
	names (AliveContainerA (a,b)) = names a
	things = \wrld -> map AliveContainerA (intersectId (things wrld :: [AliveA]) (things wrld :: [ContainerA]))
	objVolume (AliveContainerA (a,b)) = objVolume a
	parent (AliveContainerA (a,b)) = parent a
	setParent (AliveContainerA (a,b)) = \p -> AliveContainerA $ (setParent a p,setParent b p) 
	setThings = setAliveContainerAs
\end{code}

The def of setThings is somewhat more complicated.
It essentially requires that we can merge the changes accumulated in our double object with the original.
Similarly we have issues with our previous assumption of elemenating every object of the associated data type.
The problem here is that we have two more general data types, 
Whereas previously any data type in the single AnonObject must satisfy the constraint. Now we have no such condtion.
The only solution I can see is to match ids on the things function
I.e. we clear the world based on the things function and then replace them with merged things

Also, A lot of fromJusts in the following code, we should probably replace them 


\begin{code}

setAliveContainerAs :: World -> [AliveContainerA] -> World
setAliveContainerAs wrld list =
	let --this is could use cleaning up
		idList :: [Id]
		idList = map idn (things wrld :: [AliveContainerA]) 
		clearW :: World
		clearW = foldr (\idt w -> worldDObj idt w) wrld idList
		addedObjW :: World
		addedObjW = foldr (\idt w -> fromJust $ worldAppId wrld (addMergeObject w list) idt ) clearW idList
		mergeAlW :: World -> World
		mergeAlW w = foldr (\idt w' -> fromJust $ worldAppId w' (mergeAliveA w') idt ) w idList
		mergeConW :: World -> World
		mergeConW w = foldr (\idt w' -> fromJust $ worldAppId w' (mergeContainerA w') idt ) w idList
	in mergeAlW . mergeConW $ addedObjW

--these could all be a class, maybe

addMergeObject :: Object a => World -> [a] -> ObjectA -> World
addMergeObject wrld list objA = worldAObj ( mergeObject ( head . filter (((idn objA) ==) . idn) $ list ) objA ) wrld

mergeAliveA :: World -> AliveA -> World
mergeAliveA wrld alA = worldRObj (fromJust $ worldAppId wrld fn (idn alA) ) wrld
	where
		fn :: AliveA -> AliveA
		fn = (\a -> mergeAlive a alA )

mergeContainerA :: World -> ContainerA -> World
mergeContainerA wrld conA = worldRObj (fromJust $ worldAppId wrld fn (idn conA) ) wrld
	where
		fn :: ContainerA -> ContainerA
		fn = (\a -> mergeContainer a conA )

	
--it turns out it seems easier to have these as seperate functions
mergeContainer :: (Container c,Container a) => a -> c -> c
mergeContainer alCon c =
	(\x -> setInventory x (inventory alCon) ) $ c

mergeAlive :: (Alive c,Alive a) => a -> c -> c
mergeAlive alCon c =
	(\x -> setIntent x (intent alCon) ) $ c

mergeObject :: (Object c,Object a) => a -> c -> c
mergeObject alCon c =
	(\x -> setParent x (parent alCon) ) .
	(\x -> setLoc x    (loc    alCon) ) $ c



instance Alive AliveContainerA where
	intent (AliveContainerA (a,b)) = intent a
	setIntent (AliveContainerA (a,b)) = \i -> AliveContainerA (setIntent a i,b)
	player (AliveContainerA (a,b)) = player a
	
instance Container AliveContainerA where
	inventory (AliveContainerA (a,b)) = inventory b
	setInventory (AliveContainerA (a,b)) = \i -> AliveContainerA $ (a,setInventory b i)
	checkInventory (AliveContainerA (a,b)) = checkInventory b
	capacity (AliveContainerA (a,b)) = capacity b
	armSpan (AliveContainerA (a,b)) = armSpan b
	containerNames (AliveContainerA (a,b)) = containerNames b
	locked (AliveContainerA (a,b)) = locked b

\end{code}

Now ideally we would be able to write some sort of function for this.
That takes two anonymous datatypes and provides generic instance defs in line with above
the problem is that I think we lead into type level lambda again. :|
I actually do not foresee this being a serious issue. 
At worst we can split out these sort of things into their own data file and just have a ton there
And there are other issues if we get an unseemly number of orthogonal classes which need combining.


The main problem with our current setThings, is that we need some way of working out the type of our merged function. 





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
newPersonId :: Id -> Person
newPersonId id = Person 
	{pId = id
	,pLoc = Coord (0,0)
	,pVolume = Ml 66400
	,pParent = Id 0
	,pIntent = Nothing
	,pPlayer = False
	}

newPlayerId :: Id -> Person
newPlayerId id = let p = newPersonId id in
	p{pPlayer=True}


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
	,sysEvent::Maybe SysIntent
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
	,sysEvent = Nothing
	}
	
newWorldState :: State World ()
newWorldState = put newWorld

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


newPersonWorld :: State World Person
newPersonWorld = getId >>= (return . newPersonId)

newPlayerWorld = getId >>= (return .  newPlayerId)


\end{code}



