module Model where

import Graphics.Gloss  -- Import this to use `Picture`
import System.Random (randomRIO) -- For random number generation

data InfoToShow = ShowNothing
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

-- Bullet data type with a different name for the position field
data Bullet = Bullet { bulletPosition :: (Float, Float) }  -- Bullet position

-- Enemy data type with a position and picture
-- Your existing definition of Enemy
data Enemy = Enemy { enemyPosition :: (Float, Float), enemyPic :: Picture }

-- Implement Eq for Enemy
-- Implement Eq for Enemy
instance Eq Enemy where
  (Enemy (x1, y1) _) == (Enemy (x2, y2) _) = (x1 == x2) && (y1 == y2)



-- Assuming GameMode is something like this:
data GameMode = PreGame | InGame | GameOver | ControlsScreen
  deriving (Eq)  -- This automatically derives Eq instance

data GameState = GameState {
                   infoToShow        :: InfoToShow,
                   elapsedTime       :: Float,
                   position          :: (Float, Float),
                   activeKeys        :: [Char],
                   characterPic      :: Picture,
                   bullets           :: [Bullet],
                   enemies           :: [Enemy],
                   cooldownTime      :: Float,
                   isAlive           :: Bool,
                   gameMode          :: GameMode,
                   lives             :: Int,  -- Add lives to track player lives
                   backgroundPosition :: Float,  -- Position of the first background
                   backgroundPosition2 :: Float,  -- Position of the second background
                   score             :: Int,  -- Add score to track player's score
                   explosions :: [Explosion],  -- Add existing explosions
                   paused            :: Bool,  -- New field to track paused state
                   scoreSaved        :: Bool        -- New field to track if the score has been saved

                 }


-- Initialize the game state with 2 lives
initialState :: Picture -> GameState
initialState pic = GameState ShowNothing 0 (0, 0) [] pic [] [] 0 True PreGame 2 0 900 0 [] False False



-- Function to generate a random position on the screen (example: within 0-1500 for x and 0-900 for y)
randomPosition :: IO (Float, Float)
randomPosition = do
  x <- randomRIO (-750, 750)  -- random x between -750 and 750 for the screen width
  y <- randomRIO (-450, 450)  -- random y between -450 and 450 for the screen height
  return (x, y)

data Explosion = Explosion {
    explosionPosition :: (Float, Float),
    explosionTimeLeft :: Float
}