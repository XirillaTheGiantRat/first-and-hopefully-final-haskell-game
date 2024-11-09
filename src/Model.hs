module Model where

import Graphics.Gloss  -- Import this to use `Picture`

data InfoToShow = ShowNothing
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

-- Bullet data type with a different name for the position field
data Bullet = Bullet { bulletPosition :: (Float, Float) }  -- Bullet position

data GameState = GameState {
                   infoToShow    :: InfoToShow,
                   elapsedTime   :: Float,
                   position      :: (Float, Float),  -- Position of the spaceship
                   activeKeys    :: [Char],
                   characterPic  :: Picture,
                   bullets       :: [Bullet]  -- List of bullets
                 }

-- Initialize the game state
initialState :: Picture -> GameState
initialState pic = GameState ShowNothing 0 (0, 0) [] pic []  -- Start with no text or number, no bullets
