module Game.Types where

data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Eq, Show)
type Point = (Int, Int)

data GameState = GameState
  { snake :: [Point]
  , dir   :: Direction
  , food  :: Point
  , alive :: Bool
  , score :: Int
  }

blockSize :: Int
blockSize = 20

width, height :: Int
width = 20
height = 20

screenWidth, screenHeight :: Int
screenWidth = width * blockSize
screenHeight = height * blockSize
