module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss

main :: IO ()
main = do
  -- Load the BMP image
  characterPic <- loadBMP "character.bmp"
  -- Initialize the game state with the character image
  playIO (InWindow "Spaceshooter" (1500, 900) (0, 0)) -- Window title and size
         black               -- Background color
         60                 -- Frames per second
         (initialState characterPic) -- Initial state with the image
         view               -- View function
         input              -- Event function
         step               -- Step function
