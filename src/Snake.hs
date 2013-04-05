-- Snake.hs: Snake-control logic

module Snake where

import Basic

import Data.List (nub, sort, intersect)

data SnakeState = SnakeState {
    -- | Snake's position is a list containing all its elements.
    -- The first element of this list is snake's head.
    position    :: [Point],
    direction   :: Direction,
    -- | Snake's length
    len         :: Int,
    -- | How many time snake has moved without changing directions
    iterations  :: Int
} deriving (Show, Eq)

-- | Increases snake's length by 1
increaseSnakeLength :: SnakeState -> SnakeState
increaseSnakeLength (SnakeState position direction len i) = SnakeState position direction (len + 1) i

-- | Moves snake according to it's direction
moveSnake :: SnakeState -> SnakeState
moveSnake (SnakeState position direction len i) = SnakeState (newPosition) direction len (i+1)
                               where
                                newPosition = take len
                                                   ((head position |+| directionToPoint direction) : position)
                                -- check snake's length, if newPosition contains too many elements, remove them

-- | Determines how enemy snakes move.
-- | It should "see" in the direction it is heading;
-- | once it sees us it can move toward our head
-- | if it sees an apple, it can guard it by circling it.
--
-- | Right now it just moves in a square.
snakeAI :: SnakeState -> SnakeState
snakeAI ss@(SnakeState position direction len i) 
	| i>4 && direction == North = SnakeState position East len 0
	| i>4 && direction == East  = SnakeState position South len 0
	| i>4 && direction == South = SnakeState position West len 0
	| i>4 && direction == West  = SnakeState position North len 0
	| otherwise                 = ss

-- | Basically we check if there are 2 same elements in the 'position' list
checkCollision :: [Point] -> Bool
checkCollision snakePosition = outOfBoundary || selfCollision
                where
                    mini          = head $ sort snakePosition
                    maxi          = head $ reverse $ sort snakePosition
                    outOfBoundary = (x mini < 1) || (y mini < 1) ||
                                    (x maxi > numRectsX) || (y maxi > numRectsY)
                    selfCollision = (length (nub snakePosition)) /= (length snakePosition)

-- | True if there are any points in common between the snake and enemy positions
checkEnemyCollision :: [Point] -> [Point] -> Bool
checkEnemyCollision sp esp = (length (intersect sp esp)) > 0

-- | Starting position & body
initialSnakePosition = [Point (numRectsX `div` 2) (numRectsY `div` 2)]
initialEnemySnakePosition = [Point (numRectsX `div` 2 - 10) (numRectsY `div` 2 - 10)]

initialSnakeState = SnakeState initialSnakePosition North 5 0 -- snake starts at length 5
initialEnemySnakeState = SnakeState initialEnemySnakePosition South 5 0 -- snake starts at length 5

snakeEatsApple :: [Point] -> Point -> Bool
snakeEatsApple snakePosition applePosition = applePosition `elem` snakePosition

-- | Don't allow to change to opposite direction immediately
changeSnakeDirection :: SnakeState -> Direction -> SnakeState
changeSnakeDirection snakeState dir
            | oppositeDirections dir currentDirection = snakeState
            | otherwise    = snakeState { direction = dir }
        where
            currentDirection = direction snakeState
