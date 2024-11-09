module Model where

import Graphics.Gloss  -- Import this to use `Picture`

data InfoToShow = ShowNothing
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow,
                   elapsedTime :: Float,
                   position    :: (Float, Float),  -- (x, y) coordinates for the character
                   activeKeys  :: [Char],          -- List of currently held keys
                   characterPic :: Picture         -- Loaded picture for the character
                 }

initialState :: Picture -> GameState
initialState pic = GameState (ShowAChar '.') 0 (0, 0) [] pic  -- Show a character initially

