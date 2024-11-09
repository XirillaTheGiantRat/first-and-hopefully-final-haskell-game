module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = 
  case gameMode gstate of
    PreGame -> 
      -- Show the "Start Game" button in the pre-game screen
      pictures [ 
        translate (-30) (-10) $ color blue $ rectangleSolid 200 100,  -- Button background
        translate (-100) (-20) $ color white $ scale 0.2 0.2 (text "Start Game")  -- Button text
      ]
      
    InGame -> 
      if isAlive gstate
      then
        let (x, y) = position gstate
            character = translate x y $ scale 0.5 0.5 (characterPic gstate)
            -- Render each bullet (using a small red circle)
            bulletsPics = map (translateBulletPic . bulletPosition) (bullets gstate)
            translateBulletPic (bx, by) = translate bx by (color red (circleSolid 5))
            -- Render each enemy, making it slightly larger with scale
            enemiesPics = map (\(Enemy (ex, ey) epic) -> translate ex ey $ scale 1.2 1.2 epic) (enemies gstate)
            -- Display remaining lives
            livesText = translate (-700) 400 $ color white $ scale 0.2 0.2 (text ("Lives: " ++ show (lives gstate)))
        in pictures (character : bulletsPics ++ enemiesPics ++ [livesText])
      else
        -- If the player is dead, show a "Game Over" message and "Start Over" button
        pictures [
          translate (-200) 0 $ color red $ scale 0.5 0.5 (text "Game Over"),
          translate (-30) (-50) $ color blue $ rectangleSolid 200 100,  -- Button background
          translate (-100) (-60) $ color white $ scale 0.2 0.2 (text "Start Over")  -- Button text
        ]

    GameOver -> 
      pictures [
        translate (-200) 0 $ color red $ scale 0.5 0.5 (text "Game Over"),
        -- Additional UI elements for the "Game Over" screen, if any
        translate (-30) (-100) $ color blue $ rectangleSolid 200 100,  -- Button background
        translate (-100) (-110) $ color white $ scale 0.2 0.2 (text "Start Over")  -- Button text
      ]





        

