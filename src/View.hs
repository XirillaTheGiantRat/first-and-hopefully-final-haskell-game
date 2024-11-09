module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = 
  let (x, y) = position gstate
      character = translate x y (characterPic gstate)  -- Draw the character image at (x, y)
      -- Render each bullet (using a small red circle)
      bulletsPics = map (translateBulletPic . bulletPosition) (bullets gstate)  
      translateBulletPic (bx, by) = translate bx by (color red (circleSolid 5))  -- Bullet is a small red circle
  in pictures (character : bulletsPics)  -- Combine character and bullets into one picture

