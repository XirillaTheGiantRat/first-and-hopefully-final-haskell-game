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

drawScrollingBackground :: Picture -> Picture -> Float -> Float -> Picture
drawScrollingBackground topImage bottomImage yOffset1 yOffset2 = 
    let
        -- Calculate scale factors for both width and height
        scaleX = 1500 / 124  -- Screen width (1500) / image width (124)
        scaleY = 900 / 204   -- Screen height (900) / image height (204)

        -- Apply scaling to both top and bottom images
        scaledTopImage = scale scaleX scaleY topImage
        scaledBottomImage = scale scaleX scaleY bottomImage
    in
        -- Draw both images at their respective positions
        pictures [
            translate 0 yOffset2 scaledBottomImage,  -- Second image appears just after the first
            translate 0 yOffset1 scaledTopImage      -- First image moves up
        ]

view :: GameState -> IO Picture
view gstate = do
  -- Load the BMP images for background
  topImage <- loadBMP "top.bmp"
  bottomImage <- loadBMP "bottom.bmp"
  -- Load the BMP images for the lives, dead state, and the 1-heart state
  lifeImage <- loadBMP "fullhearts.bmp" 
  deadImage <- loadBMP "dead.bmp"        
  oneHeartImage <- loadBMP "1heart.bmp"  
  akRat <- loadBMP "akrat.bmp"
  akRat2 <- loadBMP "akrat2.bmp"
  explosionImage <- loadBMP "explosion.bmp"
  michaelbow <- loadBMP "michaelbow.bmp"
  catufo <- loadBMP "catufo.bmp"

  return (viewPure gstate lifeImage deadImage oneHeartImage akRat akRat2 topImage bottomImage explosionImage michaelbow catufo)

viewPure :: GameState -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture
viewPure gstate lifeImage deadImage oneHeartImage akRat akRat2 topImage bottomImage explosionImage michaelbow catufo = 
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

        -- Welcome screen
        translate (-30) (110) $ color white $ rectangleSolid 800 100,  -- Button background
        translate (-300) (120) $ color black $ scale 0.2 0.2 (text "Welcome to Michael's Ratventures Pt2! <3"),  -- Button text
        translate (-30) (130) $ thickRectangle 0 0 800 100 5 (makeColor (249/255) (156/255) (196/255) 1)
      ]
      
    InGame -> 
      if isAlive gstate
      then
        let (x, y) = position gstate
            character = translate x y $ scale 0.5 0.5 (characterPic gstate)
            bulletsPics = map (translateBulletPic . bulletPosition) (bullets gstate)
            translateBulletPic (bx, by) = translate bx by (color red (circleSolid 5))
            enemiesPics = map (\(Enemy (ex, ey) epic) -> translate ex ey $ scale 1.2 1.2 epic) (enemies gstate)
            lifePic = case lives gstate of
                        0 -> renderDead deadImage
                        1 -> renderLives oneHeartImage
                        _ -> renderLives lifeImage
            background = drawScrollingBackground topImage bottomImage (backgroundPosition gstate) (backgroundPosition2 gstate)
            scoreDisplay = translate (-600) 400 $ scale 0.3 0.3 $ color white $ text ("Score: " ++ show (score gstate))  -- Display score
            explosionsPics = [translate ex ey (scale 0.5 0.5 explosionImage) | Explosion (ex, ey) _ <- explosions gstate]
            pausedPic = if paused gstate
             then
               -- Combine the control instructions and the "Paused" box
                              translate 0 100 (color white (rectangleSolid 600 600)) <>  -- White box
                              translate (-130) 300 (color black $ scale 0.2 0.2 (text "Paused")) <>  -- Text inside the box
                              translate (-130) 250 (color black $ scale 0.2 0.2 (text "Controls:")) <>  -- Controls header
                              translate (-130) 200 (color black $ scale 0.2 0.2 (text "W - Move Up")) <>  -- Controls instructions
                              translate (-130) 150 (color black $ scale 0.2 0.2 (text "S - Move Down")) <> 
                              translate (-130) 100 (color black $ scale 0.2 0.2 (text "A - Move Left")) <> 
                              translate (-130) 50 (color black $ scale 0.2 0.2 (text "D - Move Right")) <> 
                              translate (-130) 0 (color black $ scale 0.2 0.2 (text "F - Shoot")) <>
                              translate (-130) (-50) (color black $ scale 0.2 0.2 (text "P - Pause"))

             else blank
        in pictures (background : character : bulletsPics ++ enemiesPics ++ explosionsPics ++ [lifePic, scoreDisplay, pausedPic])

      else
        -- If the player is dead, show a "Game Over" message and "Start Over" button
        pictures [
          translate (-200) (100) $ color black $ scale 0.5 0.5 (text "Game Over"),
          translate (-30) (120) $ thickRectangle 0 0 600 100 5 (makeColor (249/255) (156/255) (196/255) 1),

          translate (-30) (-120) $ color white $ rectangleSolid 200 100,  -- Button background
          translate (-100) (-130) $ color black $ scale 0.2 0.2 (text "Main Menu"),  -- Button text
          translate (-30) (-120) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),

          translate (-30) (-10) $ color white $ rectangleSolid 200 100,  -- Button background
          translate (-100) (-20)  $ color black $ scale 0.2 0.2 (text "Start Over"),  -- Button text
          translate (-30) (-10) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),
          renderDead deadImage  -- Show the dead image on the Game Over screen
        ]

    GameOver -> 
      pictures [
        translate (-200) (100) $ color black $ scale 0.5 0.5 (text "Game Over"),
        translate (-30) (120) $ thickRectangle 0 0 600 100 5 (makeColor (249/255) (156/255) (196/255) 1),

        -- Display final score
        translate (-200) 200 $ scale 0.3 0.3 $ color (makeColor (249/255) (156/255) (196/255) 1) $ text ("Final Score: " ++ show (score gstate)),  -- Display final score

        -- Main Menu button
        translate (-30) (-120) $ color white $ rectangleSolid 200 100,  -- Button background
        translate (-100) (-130) $ color black $ scale 0.2 0.2 (text "Main Menu"),  -- Button text
        translate (-30) (-120) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),

        -- Start Over button
        translate (-30) (-10) $ color white $ rectangleSolid 200 100,  -- Button background
        translate (-100) (-20)  $ color black $ scale 0.2 0.2 (text "Start Over"),  -- Button text
        translate (-30) (-10) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),

        renderDead deadImage  -- Show the dead image on the Game Over screen
      ]

    ControlsScreen -> 
      pictures [
        translate (-130) 250 $ color black $ scale 0.2 0.2 (text "Controls:"),
        translate (-130) 200 $ color black $ scale 0.2 0.2 (text "W - Move Up"),
        translate (-130) 150 $ color black $ scale 0.2 0.2 (text "S - Move Down"),
        translate (-130) 100 $ color black $ scale 0.2 0.2 (text "A - Move Left"),
        translate (-130) 50 $ color black $ scale 0.2 0.2 (text "D - Move Right"),
        translate (-130) 0 $ color black $ scale 0.2 0.2 (text "F - Shoot"),
        translate (-130) (-50) $ color black $ scale 0.2 0.2 (text "P - Pause"),
        translate (-30) (-100) $ color white $ rectangleSolid 200 100,  -- Back Button background
        translate (-60) (-130) $ color black $ scale 0.2 0.2 (text "Back"),  -- Back Button text
        translate (-30) (-120) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),
        renderAKRat akRat,
        renderAKRat2 akRat2,
        translate (-175) (-400) $ color black $ scale 0.15 0.15 (text "Michael is ready to go into space!!!")  -- Additional text
        ]
    BackStory -> 
      -- BackStory game mode (render the backstory page)
      pictures [
        -- Dit ff goed spacen en sizen straks + back button tekeken
        translate (-300) 150 $ color white $ rectangleSolid 800 500,  -- Background box
        translate (-250) 200 $ color black $ scale 0.5 0.5 (text "Backstory:") ,  -- Title
        translate (-250) 150 $ color black $ scale 0.3 0.3 (text "After you helped Michael in Michael's Ratventures (MSO)"),  -- Example text
        translate (-250) 100 $ color black $ scale 0.3 0.3 (text "find his way to his spaceship"),  -- Example continuation
        translate (-250) 50 $ color black $ scale 0.3 0.3 (text "Michael needs your help once more!"),  -- Example continuation

        translate (250) 50 $ color black $ scale 0.3 0.3 (text "The thing is Michael has never been in space!"),  -- Example text
        translate (250) 0 $ color black $ scale 0.3 0.3 (text "He has chosen you, his dearest friend,"),  -- Example continuation
        translate (250) (-50) $ color black $ scale 0.3 0.3 (text " to help him navigate the galaxy in his conquest of returning to his people."),  -- Example continuation

        translate (-250) (-100) $ color black $ scale 0.3 0.3 (text "But be aware! There are many vile creatures in space"),  -- Example text
        translate (-250) (-150) $ color black $ scale 0.3 0.3 (text "that will no doubt see Michael as a yummy snack like this cat here!"),  -- Example continuation
        translate (-250) (-200) $ color black $ scale 0.3 0.3 (text "Will you help Michael find his way home? "),  -- Example continuation
        rendercatUfo catufo,
        renderMichaelBow michaelbow,
        -- Back button (go back to PreGame or another mode)
        translate (-30) (-200) $ color white $ rectangleSolid 200 100,  -- Button background
        translate (-60) (-210) $ color black $ scale 0.2 0.2 (text "Back"),  -- Button text
        translate (-30) (-200) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1)  -- Button border
      ]
      


-- Render the full life image at a fixed position
renderLives :: Picture -> Picture
renderLives lifeImage = translate (-700) 400 lifeImage

-- Render the dead image at a fixed position (same as the life image)
renderDead :: Picture -> Picture
renderDead deadImage = translate (-700) 400 deadImage

-- Render AKRat at a fixed position
renderAKRat :: Picture -> Picture
renderAKRat akRat = translate (-450) (-200) $ scale 0.9 0.9 akRat

-- Render AKRat2 at a fixed position
renderAKRat2 :: Picture -> Picture
renderAKRat2 akRat2 = translate (450) (-200) $ scale 0.9 0.9 akRat2

-- Render AKRat2 at a fixed position
rendercatUfo :: Picture -> Picture
rendercatUfo catufo = translate (-450) (-200) $ scale 0.9 0.9 catufo

-- Render AKRat2 at a fixed position
renderMichaelBow :: Picture -> Picture
renderMichaelBow michaelbow = translate (-450) (200) $ scale 0.9 0.9 michaelbow
