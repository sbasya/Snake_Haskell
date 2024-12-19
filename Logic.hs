module Game.Logic
  ( initialState
  , updateGame
  , moveSnake
  , checkCollision
  ) where

import Game.Types
import System.Random (randomRIO)

-- Initialize the game state
initialState :: IO GameState
initialState = do
  foodPos <- randomPosition
  return $ GameState
    { snake = [(10, 10), (9, 10), (8, 10)]  -- Initial snake position
    , dir = DirRight                        -- Initial direction
    , food = foodPos                        -- Random food position
    , alive = True                          -- Snake starts alive
    , score = 0                             -- Initial score
    }

-- Generate a random position for food
randomPosition :: IO Point
randomPosition = do
  x <- randomRIO (0, width - 1)
  y <- randomRIO (0, height - 1)
  return (x, y)

-- Update the game state
updateGame :: Float -> GameState -> IO GameState
updateGame _ g@GameState{alive = False} = return g  -- If the snake is dead, do nothing
updateGame _ g@GameState{snake = s, dir = d, food = f, score = sc} = do
  let newSnake = moveSnake d s
  let ateFood = head newSnake == f
  newFood <- if ateFood then randomPosition else return f
  let growSnake = if ateFood then head newSnake : newSnake else newSnake
  let newScore = if ateFood then sc + 1 else sc
  return g
    { snake = growSnake
    , food = newFood
    , alive = not (checkCollision g{snake = growSnake})
    , score = newScore
    }

moveSnake :: Direction -> [Point] -> [Point]
moveSnake d (x:xs) = newHead : init (x:xs) -- Move the head and shift the body
  where
    newHead = case d of
      DirUp    -> (fst x, snd x + 1)    -- Move up
      DirDown  -> (fst x, snd x - 1)    -- Move down
      DirLeft  -> (fst x - 1, snd x)    -- Move left
      DirRight -> (fst x + 1, snd x)    -- Move right

checkCollision :: GameState -> Bool
checkCollision GameState{snake = s@(h:_), alive = a}
  | not a = True  -- If the snake is already dead, it stays dead
  | outOfBounds h = True  -- Collision with walls
  | h `elem` tail s = True  -- Collision with itself
  | otherwise = False
  where
    -- Check if the head is out of bounds
    outOfBounds (x, y) = x < 0 || x >= width || y < 0 || y >= height
