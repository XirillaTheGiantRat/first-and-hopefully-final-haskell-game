module Controller where

import Model
import Graphics.Gloss.Interface.IO.Game

-- Movement amount per frame
moveAmount :: Float
moveAmount = 2

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = 
  let (x, y) = position gstate
      -- Calculate new position based on active keys
      newPosition = foldl move (x, y) (activeKeys gstate)
  in return $ gstate { elapsedTime = elapsedTime gstate + secs, position = newPosition }

-- Update position based on key direction
move :: (Float, Float) -> Char -> (Float, Float)
move (x, y) 'w' = (x, y + moveAmount)  -- Move up
move (x, y) 'a' = (x - moveAmount, y)  -- Move left
move (x, y) 's' = (x, y - moveAmount)  -- Move down
move (x, y) 'd' = (x + moveAmount, y)  -- Move right
move pos _      = pos                  -- Ignore other keys

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate
  | c `elem` "wasd" = gstate { activeKeys = c : activeKeys gstate }  -- Add key to activeKeys on press
inputKey (EventKey (Char c) Up _ _) gstate
  | c `elem` "wasd" = gstate { activeKeys = filter (/= c) (activeKeys gstate) }  -- Remove key on release
inputKey _ gstate = gstate  -- Keep state unchanged for other events
