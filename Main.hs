module Main where

import Graphics.Gloss.Interface.IO.Game
import Game.Types
import Game.Logic
import Game.Renderer
import Game.Input

main :: IO ()
main = do
  initial <- initialState
  playIO
    (InWindow "Snake Game" (screenWidth, screenHeight) (100, 100))
    black  -- Background color
    5      -- Frames per second
    initial
    (return . drawGame)  -- Adapt drawGame to IO Picture
    handleInput
    updateGame
