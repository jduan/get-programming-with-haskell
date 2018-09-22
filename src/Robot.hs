module Robot where

-- This is a constructor. It takes a tuple and returns a function
-- that takes another function to act on the tuple.
robot (name, attack, hp) = \message -> message (name, attack, hp)

killerRobot = robot ("Kill3r", 25, 200)

name (n, _, _) = n

attack (_, a, _) = a

hp (_, _, hp) = hp

-- getters
getName aRobot = aRobot name

getAttack aRobot = aRobot attack

getHP aRobot = aRobot hp

-- setters
setName aRobot newName = robot (newName, getAttack aRobot, getHP aRobot)

setAttack aRobot newAttack = robot (getName aRobot, newAttack, getHP aRobot)

setHP aRobot newHP = robot (getName aRobot, getAttack aRobot, newHP)

printRobot aRobot =
  aRobot (\(n, a, h) -> n ++ " attack:" ++ show a ++ " hp:" ++ show h)
