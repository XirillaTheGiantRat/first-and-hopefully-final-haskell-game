module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss

main :: IO ()
main = do
  -- load the highscores
  highScores <- readHighScores
  -- Load the BMP image for the spaceship
  -- Load the BMP image for the spaceship and the enemies
  characterPic <- loadBMP "character.bmp"
  enemyPic <- loadBMP "enemies.bmp"  -- Load enemy image
  
  -- Initialize the game state with the character image and no enemies
  let initialState' = initialState characterPic  -- Start with no enemies initially
  
  -- Start the game with initial state
  playIO (InWindow "Spaceshooter" (1500, 900) (0, 0)) -- Window title and size
         black               -- Background color
         60                  -- Frames per second
         initialState'       -- Initial state with the character image
         view                -- View function
         input               -- Event function
         step                -- Step function
