module Game.Input where

import Graphics.Gloss.Interface.IO.Game -- Ensure Down is in scope
import Game.Types (GameState(..), Direction(..))

handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (SpecialKey KeyUp) Down _ _) g@GameState{dir = d}
  | d /= DirDown = return g{dir = DirUp}
handleInput (EventKey (SpecialKey KeyDown) Down _ _) g@GameState{dir = d}
  | d /= DirUp = return g{dir = DirDown}
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) g@GameState{dir = d}
  | d /= DirRight = return g{dir = DirLeft}
handleInput (EventKey (SpecialKey KeyRight) Down _ _) g@GameState{dir = d}
  | d /= DirLeft = return g{dir = DirRight}
handleInput _ g = return g
