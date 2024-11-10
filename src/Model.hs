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
                   paused            :: Bool,  
                   scoreSaved        :: Bool       
                 }

-- initial data
initialState :: Picture -> GameState
initialState pic = GameState ShowNothing 0 (0, 0) [] pic [] [] 0 True PreGame 2 0 900 0 [] False False

data Explosion = Explosion {
    explosionPosition :: (Float, Float),
    explosionTimeLeft :: Float
}