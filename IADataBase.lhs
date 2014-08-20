
\begin{code}
{-# LANGUAGE ExistentialQuantification  #-}


module IADataBase where

import Data.Maybe

\end{code}


Merged data types from previous version.

\begin{code}

data InterruptType 
	= Sight --the thing is visible
	| Touch Id --the thing is touching id.
	| Sound String --the thing is making noise, described by String. i.e. string can be used for speech or general sound descriptions.
	deriving (Eq,Show)

newtype Interrupt = InterruptType (Id,Integer)

data AbsDirection = North | South | East | West | Here | Up | Down deriving (Eq, Show)

data RelDirection = In | Out | On | Below deriving (Eq, Show)

data Direction = Rel RelDirection | Abs AbsDirection deriving (Eq, Show)

data SysIntent = Quit | Help | VerNum | Setting  deriving (Eq,Show)

data Settings = Volume | Difficulty deriving (Eq,Show) 

data SysSetting = SysSetting (Int,Settings)  deriving (Eq,Show)

checkSetting :: [SysSetting] -> Settings -> Int
checkSetting list s = foldr (\(SysSetting (b,c)) a -> if c == s then b else a ) (-1) list

getSetting = (\(SysSetting (b,c))-> c)

changeSetting :: [SysSetting] -> SysSetting -> [SysSetting]
changeSetting list sett =
	if elem (getSetting sett) $ map (\(SysSetting (b,c)) -> c) list 
		then  foldr (\a list' -> if (getSetting a) == (getSetting sett) then sett : list' else a : list' ) [] list
		else sett : list

data Intent = SysCom SysIntent | Move Target | Get Id | Look Target deriving (Eq,Show)

data Target = Tar (Maybe Direction) Id deriving (Eq, Show)

\end{code}


Some basic Data types 

\begin{code}

newtype Id = Id Int deriving (Show,Eq) --idk if this should just be Ix

newtype Coord = Coord (Integer,Integer) deriving Eq

newtype Ml = Ml Integer deriving (Eq, Ord) --millilitres

newtype Mm = Mm Integer deriving (Eq,Ord) --millimetres

newtype Gram = Gram Integer deriving (Eq,Ord)

eucDistSqrd (Coord (x,y)) (Coord (u,v)) = (x-u)^2 +(y-v)^2

manAdj (Coord (x,y)) = [Coord (x - 1,y),Coord (x+1,y),Coord (x,y - 1),Coord (x,y+1)]

\end{code}
