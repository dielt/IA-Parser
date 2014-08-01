module IASyn where


waitSyn = 
	["wait"
	,"pause"
	]
fillerWords = 
	["at"
	,"the"
	,"to"
	,"a"
	]
negSyn = 
	["no"
	,"n"
	,"nope"
	,"not"
	]
affirmSyn =
	["yes"
	,"y"
	,"yep"
	,"yea"
	,"yeah"
	,"affirmative"
	]
quitSyn = 
	["quit"
	,"q"
	,"exit"
	]
helpSyn = 
	["help"
	,"h"
	,"?"
	]
	
dirRelSyn = inSyn ++ outSyn ++ onSyn ++ belowSyn
dirSyn = cardinalSyn ++ hereSyn ++ upSyn ++ downSyn ++ inSyn ++ outSyn ++ onSyn ++ belowSyn
cardinalSyn = northSyn ++ southSyn ++ eastSyn ++ westSyn

hereSyn =
	["here"
	,"around"
	,"nearby"
	]
northSyn = 
	["north"
	,"n"
	]
southSyn = 
	["south"
	,"s"
	]
eastSyn =
	["east"
	,"e"
	]
westSyn = 
	["west"
	,"w"
	]
upSyn = 
	["up"
	,"u"
	,"above"
	]
downSyn =
	["down"
	,"d"
	,"below"
	]
inSyn =
	["in"
	,"inside"
	,"within"
	,"from"
	,"i"
	,"indoors"
	,"through"
	]
outSyn = 
	["out"
	]
belowSyn =
	["below"
	,"under"
	,"beneath"
	,"underneath"
	]
onSyn =
	["on"
	,"above"
	,"from"
	,"top"
	]
moveSyn = 
	["move"
	,"go"
	,"get"
	]
lookSyn =
	["look"
	,"examine"
	,"x"
	,"l"
	]
getSyn = 
	["get"
	,"grab"
	,"g"
	]
selfSyn =
	["me"
	,"i"
	,"myself"
	]
invSyn = 
	["i"
	,"inventory"
	,"inv"
	]


