module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = 
  let (x, y) = position gstate
      dot = translate x y . color blue . scale 0.1 0.1 $ text "."  -- Scale down the dot for size
  in case infoToShow gstate of
      ShowNothing   -> blank
      ShowANumber n -> color blue (text (show n))
      ShowAChar   _ -> dot  -- Display the dot at (x, y)
