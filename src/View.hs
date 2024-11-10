module View where

import Graphics.Gloss
import Model

--border for buttons
thickRectangle :: Float -> Float -> Float -> Float -> Float -> Color -> Picture
thickRectangle xPos yPos width height thickness borderColor =
  pictures [ 
    color borderColor $ rectangleWire (width + offset) (height + offset)
    | offset <- [0, thickness .. thickness * 3]  
  ]

drawScrollingBackground :: Picture -> Picture -> Float -> Float -> Picture
drawScrollingBackground topImage bottomImage yOffset1 yOffset2 = 
    let
        scaleX = 1500 / 124  
        scaleY = 900 / 204   

        scaledTopImage = scale scaleX scaleY topImage
        scaledBottomImage = scale scaleX scaleY bottomImage
    in
        pictures [
            translate 0 yOffset2 scaledBottomImage,  
            translate 0 yOffset1 scaledTopImage    
        ]

view :: GameState -> IO Picture
view gstate = do
  -- Loading bmps
  topImage <- loadBMP "top.bmp"
  bottomImage <- loadBMP "bottom.bmp"
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
      pictures [ 
        -- Start Game Button
        translate (-30) (-10) $ color white $ rectangleSolid 200 100,  
        translate (-100) (-20) $ color black $ scale 0.2 0.2 (text "Start Game"),  
        translate (-30) (-10) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),

        -- Controls Button
        translate (-30) (-120) $ color white $ rectangleSolid 200 100,  
        translate (-80) (-130) $ color black $ scale 0.2 0.2 (text "Controls"),  
        translate (-30) (-120) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),

        -- Backstory buttons
        translate (-30) (-230) $ color white $ rectangleSolid 200 100,  
        translate (-90) (-240) $ color black $ scale 0.2 0.2 (text "Backstory"), 
        translate (-30) (-230) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),

        -- Welcome screen
        translate (-30) (110) $ color white $ rectangleSolid 800 100, 
        translate (-300) (120) $ color black $ scale 0.2 0.2 (text "Welcome to Michael's Ratventures Pt. 2! <3"), 
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
            scoreDisplay = translate (-600) 400 $ scale 0.3 0.3 $ color white $ text ("Score: " ++ show (score gstate))  
            explosionsPics = [translate ex ey (scale 0.5 0.5 explosionImage) | Explosion (ex, ey) _ <- explosions gstate]
            pausedPic = if paused gstate
             then
                              -- Pause menu
                              translate 0 100 (color white (rectangleSolid 600 600)) <>  
                              translate (-130) 300 (color black $ scale 0.2 0.2 (text "Paused")) <>  
                              translate (-130) 250 (color black $ scale 0.2 0.2 (text "Controls:")) <> 
                              translate (-130) 200 (color black $ scale 0.2 0.2 (text "W - Move Up")) <>  
                              translate (-130) 150 (color black $ scale 0.2 0.2 (text "S - Move Down")) <> 
                              translate (-130) 100 (color black $ scale 0.2 0.2 (text "A - Move Left")) <> 
                              translate (-130) 50 (color black $ scale 0.2 0.2 (text "D - Move Right")) <> 
                              translate (-130) 0 (color black $ scale 0.2 0.2 (text "F - Shoot")) <>
                              translate (-130) (-50) (color black $ scale 0.2 0.2 (text "P - Pause"))

             else blank
        in pictures (background : character : bulletsPics ++ enemiesPics ++ explosionsPics ++ [lifePic, scoreDisplay, pausedPic])

      else
        pictures [
          translate (-200) (100) $ color black $ scale 0.5 0.5 (text "Game Over"),
          translate (-30) (120) $ thickRectangle 0 0 600 100 5 (makeColor (249/255) (156/255) (196/255) 1),

          translate (-30) (-120) $ color white $ rectangleSolid 200 100,  
          translate (-100) (-130) $ color black $ scale 0.2 0.2 (text "Main Menu"), 
          translate (-30) (-120) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),

          translate (-30) (-10) $ color white $ rectangleSolid 200 100,  
          translate (-100) (-20)  $ color black $ scale 0.2 0.2 (text "Start Over"), 
          translate (-30) (-10) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),
          renderDead deadImage  
        ]

    GameOver -> 
      pictures [
        translate (-200) (100) $ color black $ scale 0.5 0.5 (text "Game Over"),
        translate (-30) (120) $ thickRectangle 0 0 600 100 5 (makeColor (249/255) (156/255) (196/255) 1),

        -- Final score
        translate (-200) 200 $ scale 0.3 0.3 $ color (makeColor (249/255) (156/255) (196/255) 1) $ text ("Final Score: " ++ show (score gstate)),  -- Display final score

        -- Main Menu button
        translate (-30) (-120) $ color white $ rectangleSolid 200 100,  
        translate (-100) (-130) $ color black $ scale 0.2 0.2 (text "Main Menu"),  
        translate (-30) (-120) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),

        -- Start Over button
        translate (-30) (-10) $ color white $ rectangleSolid 200 100,  
        translate (-100) (-20)  $ color black $ scale 0.2 0.2 (text "Start Over"), 
        translate (-30) (-10) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),

        renderDead deadImage  
      ]

    ControlsScreen -> 
      pictures [
        -- controls
        translate (-130) 250 $ color black $ scale 0.2 0.2 (text "Controls:"),
        translate (-130) 200 $ color black $ scale 0.2 0.2 (text "W - Move Up"),
        translate (-130) 150 $ color black $ scale 0.2 0.2 (text "S - Move Down"),
        translate (-130) 100 $ color black $ scale 0.2 0.2 (text "A - Move Left"),
        translate (-130) 50 $ color black $ scale 0.2 0.2 (text "D - Move Right"),
        translate (-130) 0 $ color black $ scale 0.2 0.2 (text "F - Shoot"),
        translate (-130) (-50) $ color black $ scale 0.2 0.2 (text "P - Pause"),
        translate (-30) (-100) $ color white $ rectangleSolid 200 100, 
        translate (-60) (-130) $ color black $ scale 0.2 0.2 (text "Back"),  
        translate (-30) (-120) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1),
        renderAKRat akRat,
        renderAKRat2 akRat2,
        translate (-175) (-400) $ color black $ scale 0.15 0.15 (text "Michael is ready to go into space!!!") 
        ]
    BackStory -> 
      pictures [
        translate (-400) 350 $ color white $ rectangleSolid 800 500,  
        translate (-400) 350 $ color black $ scale 0.5 0.5 (text "Backstory:") ,  
        translate (-400) 300 $ color black $ scale 0.3 0.3 (text "After you helped Michael in Michael's Ratventures (MSO)"),  
        translate (-400) 250 $ color black $ scale 0.3 0.3 (text "find his way to his spaceship"),  
        translate (-400) 200 $ color black $ scale 0.3 0.3 (text "Michael needs your help once more!"), 

        translate (-350) 100 $ color black $ scale 0.3 0.3 (text "The thing is Michael has never been in space!"),  
        translate (-350) 50 $ color black $ scale 0.3 0.3 (text "He has chosen you, his dearest friend,"),  
        translate (-350) 0 $ color black $ scale 0.3 0.3 (text "to help him navigate the galaxy"),  
        translate (-350) (-50) $ color black $ scale 0.3 0.3 (text "in his conquest of returning to his people."), 

        translate (-700) (-150) $ color black $ scale 0.3 0.3 (text "But be aware! There are many vile creatures in space that will"), 
        translate (-700) (-200) $ color black $ scale 0.3 0.3 (text "no doubt see Michael as a yummy snack like this cat here!"), 
        translate (-700) (-250) $ color black $ scale 0.3 0.3 (text "Will you help Michael find his way home? "),  
        rendercatUfo catufo,
        renderMichaelBow michaelbow,
        -- Back button 
        translate (-30) (-340) $ color white $ rectangleSolid 200 100, 
        translate (-60)(-350) $ color black $ scale 0.2 0.2 (text "Back"),  
        translate (-30) (-340) $ thickRectangle 0 0 200 100 5 (makeColor (249/255) (156/255) (196/255) 1) 
      ]
      


-- Render 2 hearts
renderLives :: Picture -> Picture
renderLives lifeImage = translate (-700) 400 lifeImage

-- Render 0 hearts
renderDead :: Picture -> Picture
renderDead deadImage = translate (-700) 400 deadImage

-- Render AKRat 
renderAKRat :: Picture -> Picture
renderAKRat akRat = translate (-450) (-200) $ scale 0.9 0.9 akRat

-- Render mirrored AKRat
renderAKRat2 :: Picture -> Picture
renderAKRat2 akRat2 = translate (450) (-200) $ scale 0.9 0.9 akRat2

-- Render catufo
rendercatUfo :: Picture -> Picture
rendercatUfo catufo = translate (550) (-300) $ scale 0.9 0.9 catufo

-- Render michael with a cutesy bow
renderMichaelBow :: Picture -> Picture
renderMichaelBow michaelbow = translate (-550) (100) $ scale 0.9 0.9 michaelbow
