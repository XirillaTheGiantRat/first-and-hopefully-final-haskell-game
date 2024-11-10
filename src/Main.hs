module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss

main :: IO ()
main = do
  -- Load the BMPs
  characterPic <- loadBMP "character.bmp"
  enemyPic <- loadBMP "enemies.bmp" 
  
  let initialState' = initialState characterPic  
  
  playIO (InWindow "Michael's Ratventures Pt. 2" (1500, 900) (0, 0))
         white               -- Background color
         60                  -- Frames per second
         initialState'       -- Initial state with the character image
         view                -- View function
         input               -- Event function
         step                -- Step function

