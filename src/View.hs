module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = 
  let (x, y) = position gstate
      character = translate x y (characterPic gstate)  -- Move the character based on position
  in case infoToShow gstate of
      ShowNothing   -> blank           -- Don't show anything if we don't need to display anything
      ShowAChar _   -> character       -- Show character if we need to display a character

