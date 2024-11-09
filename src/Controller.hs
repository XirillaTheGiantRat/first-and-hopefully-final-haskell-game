module Controller where

import Model
import Graphics.Gloss.Interface.IO.Game

-- Movement amount per key press
moveAmount :: Float
moveAmount = 10

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = 
  -- Only update the elapsed time, without modifying infoToShow or position
  return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'w') Down _ _) gstate = 
    gstate { position = move (0, moveAmount) (position gstate) }
inputKey (EventKey (Char 'a') Down _ _) gstate = 
    gstate { position = move (-moveAmount, 0) (position gstate) }
inputKey (EventKey (Char 's') Down _ _) gstate = 
    gstate { position = move (0, -moveAmount) (position gstate) }
inputKey (EventKey (Char 'd') Down _ _) gstate = 
    gstate { position = move (moveAmount, 0) (position gstate) }
inputKey _ gstate = gstate  -- Keep the state unchanged for other events

-- Helper function to move the position
move :: (Float, Float) -> (Float, Float) -> (Float, Float)
move (dx, dy) (x, y) = (x + dx, y + dy)
