module Model where

import Graphics.Gloss  
import System.Random (randomRIO) 

data InfoToShow = ShowNothing
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data Bullet = Bullet { bulletPosition :: (Float, Float) }  

data Enemy = Enemy { enemyPosition :: (Float, Float), enemyPic :: Picture }

instance Eq Enemy where
  (Enemy (x1, y1) _) == (Enemy (x2, y2) _) = (x1 == x2) && (y1 == y2)

data GameMode = PreGame | InGame | GameOver | ControlsScreen | BackStory
  deriving (Eq)  

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
                   lives             :: Int,  
                   backgroundPosition :: Float,  
                   backgroundPosition2 :: Float,  
                   score             :: Int, 
                   explosions :: [Explosion],  
                   paused            :: Bool  

                 }

-- initial data
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