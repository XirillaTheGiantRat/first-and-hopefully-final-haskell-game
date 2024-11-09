module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view gstate = do
  -- Load the BMP images for the lives, dead state, and the 1-heart state
  lifeImage <- loadBMP "fullhearts.bmp"  -- Update the path as needed
  deadImage <- loadBMP "dead.bmp"        -- Update the path as needed
  oneHeartImage <- loadBMP "1heart.bmp"  -- Update the path as needed
  return (viewPure gstate lifeImage deadImage oneHeartImage)

viewPure :: GameState -> Picture -> Picture -> Picture -> Picture
viewPure gstate lifeImage deadImage oneHeartImage = 
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
            -- Render the appropriate image based on the number of lives
            lifePic = case lives gstate of
                        0 -> renderDead deadImage  -- Draw the dead image when lives are 0
                        1 -> renderLives oneHeartImage  -- Draw 1-heart image when lives are 1
                        _ -> renderLives lifeImage  -- Draw full hearts for more than 1 life
        in pictures (character : bulletsPics ++ enemiesPics ++ [lifePic])  -- Include the correct life image
        
      else
        -- If the player is dead, show a "Game Over" message and "Start Over" button
        pictures [
          translate (-200) 0 $ color red $ scale 0.5 0.5 (text "Game Over"),
          translate (-30) (-50) $ color blue $ rectangleSolid 200 100,  -- Button background
          translate (-100) (-60) $ color white $ scale 0.2 0.2 (text "Start Over"),  -- Button text
          -- Show the dead image on the Game Over screen
          renderDead deadImage
        ]

    GameOver -> 
      pictures [
        translate (-200) 0 $ color red $ scale 0.5 0.5 (text "Game Over"),
        -- Additional UI elements for the "Game Over" screen, if any
        translate (-30) (-100) $ color blue $ rectangleSolid 200 100,  -- Button background
        translate (-100) (-110) $ color white $ scale 0.2 0.2 (text "Start Over"),  -- Button text
        -- Show the dead image on the Game Over screen
        renderDead deadImage
      ]


-- Render the full life image at a fixed position
renderLives :: Picture -> Picture
renderLives lifeImage = translate (-700) 400 lifeImage

-- Render the dead image at a fixed position (same as the life image)
renderDead :: Picture -> Picture
renderDead deadImage = translate (-700) 400 deadImage
