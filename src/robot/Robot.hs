module Robot where

-- This is a constructor. It takes a tuple and returns a function
-- that takes another function to act on the tuple.
robot (name, attack, hp) = \message -> message (name, attack, hp)

killerRobot = robot ("Kill3r", 25, 200)

-- nicerRobot = setName killerRobot "kitty"
--
-- gentlerRobot = setAttack killerRobot 5
--
-- softerRobot = setHP killerRobot 50
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

-- Reduce a robot's HP by some
damage aRobot attackDamage =
  aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

-- make a robot fight another robot
fight aRobot defender = damage defender attack
  where
    attack =
      if getHP aRobot > 10
        then getAttack aRobot
        else 0

gentleGiant = robot ("Mr. Friendly", 10, 300)

gentleGiantRound1 = fight killerRobot gentleGiant

killerRobotRound1 = fight gentleGiant killerRobot

gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1

killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1

gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2

killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2

mainRobot :: IO ()
mainRobot = do
  print $ printRobot gentleGiantRound3
  print $ printRobot killerRobotRound3
