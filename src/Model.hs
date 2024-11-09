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
data Enemy = Enemy { enemyPosition :: (Float, Float), enemyPic :: Picture }  -- Represents an enemy

data GameState = GameState {
                   infoToShow    :: InfoToShow,
                   elapsedTime   :: Float,
                   position      :: (Float, Float),  -- Position of the spaceship
                   activeKeys    :: [Char],
                   characterPic  :: Picture,
                   bullets       :: [Bullet],  -- List of bullets
                   enemies       :: [Enemy]   -- List of enemies
                 }

-- Initialize the game state
initialState :: Picture -> GameState
initialState pic = GameState ShowNothing 0 (0, 0) [] pic [] []

-- Function to generate a random position on the screen (example: within 0-1500 for x and 0-900 for y)
randomPosition :: IO (Float, Float)
randomPosition = do
  x <- randomRIO (-750, 750)  -- random x between -750 and 750 for the screen width
  y <- randomRIO (-450, 450)  -- random y between -450 and 450 for the screen height
  return (x, y)
