module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow,
                   elapsedTime :: Float,
                   position    :: (Float, Float)  -- (x, y) coordinates for the dot
                 }

initialState :: GameState
initialState = GameState (ShowAChar '.') 0 (0, 0)  -- Start with a dot at the center

