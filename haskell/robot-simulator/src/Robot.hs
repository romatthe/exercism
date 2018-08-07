module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot Bearing (Integer, Integer)

bearing :: Robot -> Bearing
bearing (Robot b (_, _)) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ (x, y)) = (x, y)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot b p = Robot b p

simulate :: Robot -> String -> Robot
simulate robot instructions = foldl execute robot instructions

execute :: Robot -> Char -> Robot
execute (Robot b (x, y)) 'L'     = Robot (turnLeft b) (x, y)
execute (Robot b (x, y)) 'R'     = Robot (turnRight b) (x, y)  
execute (Robot North (x, y)) 'A' = Robot North (x, y + 1)  
execute (Robot South (x, y)) 'A' = Robot South (x, y - 1)  
execute (Robot East (x, y)) 'A'  = Robot East (x + 1, y)  
execute (Robot West (x, y)) 'A'  = Robot West (x - 1, y)  

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft South = East
turnLeft East  = North
turnLeft West  = South

turnRight :: Bearing -> Bearing
turnRight North = East
turnRight South = West
turnRight East  = South
turnRight West  = North
