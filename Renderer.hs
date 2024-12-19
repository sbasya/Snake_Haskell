module Game.Renderer where

import qualified Graphics.Gloss as Gloss  -- Qualified import
import Game.Types

-- Use Game.Types.Point explicitly
drawBlock :: Point -> Gloss.Picture
drawBlock (x, y) =
  Gloss.translate
    (fromIntegral $ x * blockSize - screenWidth `div` 2)
    (fromIntegral $ y * blockSize - screenHeight `div` 2)
    $ Gloss.rectangleSolid (fromIntegral blockSize) (fromIntegral blockSize)

drawGame :: GameState -> Gloss.Picture
drawGame GameState{snake = s, food = f, alive = a, score = sc} =
  Gloss.Pictures $ foodPic : snakePic ++ [border, scoreText]
  where
    snakeColor = if a then Gloss.green else Gloss.red
    snakePic = map (Gloss.color snakeColor . drawBlock) s
    foodPic = Gloss.color Gloss.blue $ drawBlock f
    border = Gloss.color Gloss.white $ Gloss.rectangleWire (fromIntegral screenWidth) (fromIntegral screenHeight)
    scoreText =
      Gloss.translate (-200) 200
        $ Gloss.scale 0.15 0.15
        $ Gloss.color Gloss.white
        $ Gloss.Text ("Score: " ++ show sc)
