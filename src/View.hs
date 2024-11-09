module View where

import Graphics.Gloss
import Model

-- Define a function to draw a thick border using layered rectangles
thickRectangle :: Float -> Float -> Float -> Float -> Float -> Color -> Picture
thickRectangle xPos yPos width height thickness borderColor =
  pictures [ 
    color borderColor $ rectangleWire (width + offset) (height + offset)
    | offset <- [0, thickness .. thickness * 3]  -- Adjust the multiplier to control the thickness level
  ]



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
      -- Show the "Start Game" button and "Controls" button in the pre-game screen
      pictures [ 
        -- Start Game Button
        translate (-30) (-10) $ color white $ rectangleSolid 200 100,  -- Button background
        translate (-100) (-20) $ color black $ scale 0.2 0.2 (text "Start Game"),  -- Button text
        translate (-30) (-10) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),

        
        -- Controls Button
        translate (-30) (-120) $ color white $ rectangleSolid 200 100,  -- Button background
        translate (-80) (-130) $ color black $ scale 0.2 0.2 (text "Controls"),  -- Button text
        translate (-30) (-120) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),


        --welcome screen
        translate (-30) (110) $ color white $ rectangleSolid 800 100,  -- Button background
        translate (-300) (120) $ color black $ scale 0.2 0.2 (text "Welcome to Michael's Ratventures Pt2! <3"),  -- Button text
        -- Use the thickRectangle function to draw a thick pink border
        translate (-30) (130) $ thickRectangle 0 0 800 100 5 (makeColor (249/255) (156/255) (196/255) 1)


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
          translate (-200) (100) $ color black $ scale 0.5 0.5 (text "Game Over"),
          translate (-30) (120) $ thickRectangle 0 0 600 100 5 (makeColor (249/255) (156/255) (196/255) 1),

        translate (-30) (-120) $ color white $ rectangleSolid 200 100,  -- Button background
        translate (-100) (-130) $ color black $ scale 0.2 0.2 (text "Main Menu"),  -- Button text
        translate (-30) (-120) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),

        -- Additional UI elements for the "Game Over" screen, if any
        translate (-30) (-10) $ color white $ rectangleSolid 200 100,  -- Button background
        translate (-100) (-20)  $ color black $ scale 0.2 0.2 (text "Start Over"),  -- Button text
        translate (-30) (-10) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),
          -- Show the dead image on the Game Over screen
          renderDead deadImage
        ]

    GameOver -> 
      pictures [
        translate (-200) (100) $ color black $ scale 0.5 0.5 (text "Game Over"),
        translate (-30) (120) $ thickRectangle 0 0 600 100 5 (makeColor (249/255) (156/255) (196/255) 1),

        translate (-30) (-120) $ color white $ rectangleSolid 200 100,  -- Button background
        translate (-100) (-130) $ color black $ scale 0.2 0.2 (text "Main Menu"),  -- Button text
        translate (-30) (-120) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),

        -- Additional UI elements for the "Game Over" screen, if any
        translate (-30) (-10) $ color white $ rectangleSolid 200 100,  -- Button background
        translate (-100) (-20)  $ color black $ scale 0.2 0.2 (text "Start Over"),  -- Button text
        translate (-30) (-10) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),
        -- Show the dead image on the Game Over screen
        renderDead deadImage
      ]

    ControlsScreen -> 
      -- Show controls instructions text on the Controls screen
      pictures [
        translate (-300) 200 $ color black $ scale 0.1 0.1 (text "Controls:"),
        translate (-300) 150 $ color black $ scale 0.1 0.1 (text "W - Move Up"),
        translate (-300) 100 $ color black $ scale 0.1 0.1 (text "S - Move Down"),
        translate (-300) 50 $ color black $ scale 0.1 0.1 (text "A - Move Left"),
        translate (-300) 0 $ color black $ scale 0.1 0.1 (text "D - Move Right"),
        translate (-300) (-50) $ color black $ scale 0.1 0.1 (text "F - Shoot"),
        translate (-30) (-100) $ color white $ rectangleSolid 200 100,  -- Back Button background
        translate (-60) (-130) $ color black $ scale 0.2 0.2 (text "Back"),  -- Back Button text
        translate (-30) (-120) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1)

      ]

-- Render the full life image at a fixed position
renderLives :: Picture -> Picture
renderLives lifeImage = translate (-700) 400 lifeImage

-- Render the dead image at a fixed position (same as the life image)
renderDead :: Picture -> Picture
renderDead deadImage = translate (-700) 400 deadImage
