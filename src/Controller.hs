module Controller where

import Model
import Graphics.Gloss.Interface.IO.Game
import Data.List (sortBy)
import System.IO (readFile, writeFile, appendFile)
import Graphics.Gloss
import System.Random (randomRIO)  -- For random generation of enemies

-- Movement amount per frame
moveAmount :: Float
moveAmount = 2

step :: Float -> GameState -> IO GameState
step secs gstate = do
  -- Calculate the new elapsed time
  let newElapsedTime = elapsedTime gstate + secs

-- Background scrolling
  -- Background scrolling
  let
        -- Calculate new positions for both backgrounds
        newBgPosition = backgroundPosition gstate - (100 * secs)
        newBgPosition2 = backgroundPosition2 gstate - (100 * secs)

        -- Reset the first background if it's off-screen, and spawn a new one above it
        finalBgPosition = if newBgPosition <= -900 then 900 else newBgPosition

        -- Reset the second background if it's off-screen, and spawn a new one above it
        finalBgPosition2 = if newBgPosition2 <= -900 then 900 else newBgPosition2




  -- Reduce the cooldown timer if it's greater than 0
  let newCooldownTime = max 0 (cooldownTime gstate - secs)

  -- Ensure there are always two enemies
  let enemiesToAdd = if length (enemies gstate) < 2
                     then spawnRandomEnemies 1
                     else return (enemies gstate)

  -- If the player is alive, move the character and process the bullets
  let (x, y) = position gstate
      newPosition = if isAlive gstate
                    then foldl move (x, y) (activeKeys gstate)
                    else (x, y)

  -- Move the bullets and remove those that go off screen
  let updatedBullets = filter (\(Bullet (_, by)) -> by <= 900) $ map moveBullet (bullets gstate)
  

  -- Handle continuous shooting with cooldown
  let newBullet = if 'f' `elem` activeKeys gstate && newCooldownTime == 0 && isAlive gstate
                  then [Bullet (x, y + 20)]
                  else []
      allBullets = newBullet ++ updatedBullets

  -- Update the cooldown time
  let finalCooldownTime = if 'f' `elem` activeKeys gstate && newCooldownTime == 0 && isAlive gstate
                           then 0.2
                           else newCooldownTime

  -- Get the updated enemies, filtering out those hit by bullets
  enemiesToAdd' <- enemiesToAdd
  updatedEnemies <- handleCollisions allBullets enemiesToAdd'

  -- Check for collision between the player and enemies
  let newGameState = foldl (\gs enemy -> isPlayerHitByEnemy (position gs) enemy gs) gstate updatedEnemies

  -- Return the updated game state with the new enemies, bullets, cooldown time, and game mode
  return newGameState { elapsedTime = newElapsedTime, position = newPosition, bullets = allBullets, enemies = updatedEnemies, cooldownTime = finalCooldownTime, backgroundPosition = finalBgPosition, backgroundPosition2 = finalBgPosition2}




-- Function to check if the player has collided with an enemy and update the game state
isPlayerHitByEnemy :: (Float, Float) -> Enemy -> GameState -> GameState
isPlayerHitByEnemy (px, py) (Enemy (ex, ey) _) gstate
  | isAlive gstate && abs (px - ex) < 20 && abs (py - ey) < 20 = 
      let newLives = lives gstate - 1
          newIsAlive = newLives > 0
          newGameMode = if newLives <= 0 then GameOver else gameMode gstate
      in gstate { lives = newLives, isAlive = newIsAlive, gameMode = newGameMode }
  | otherwise = gstate






-- Function to check for collisions between bullets and enemies
handleCollisions :: [Bullet] -> [Enemy] -> IO [Enemy]
handleCollisions bullets enemies = do
  let remainingEnemies = filter (not . isHitByBullet bullets) enemies
  if length remainingEnemies < 2  -- If less than 2 enemies remain after a collision
    then do
      -- Spawn a new enemy if one dies, while keeping the other
      newEnemies <- spawnRandomEnemies 1
      return $ remainingEnemies ++ newEnemies
    else return remainingEnemies  -- Otherwise, return the remaining enemies

-- Function to check if an enemy is hit by any bullet
isHitByBullet :: [Bullet] -> Enemy -> Bool
isHitByBullet bullets enemy = any (isBulletHit enemy) bullets

-- Function to check if a specific bullet hits an enemy
isBulletHit :: Enemy -> Bullet -> Bool
isBulletHit (Enemy (ex, ey) _) (Bullet (bx, by)) =
  -- Check if the bullet's position is close enough to the enemy's position
  abs (bx - ex) < 20 && abs (by - ey) < 20  -- Adjust 20 to the appropriate "hit radius"

screenWidth :: Float
screenWidth = 400.0  -- Replace with your screen's width as Float

-- Function to spawn a certain number of enemies at random positions
spawnRandomEnemies :: Int -> IO [Enemy]
spawnRandomEnemies n = do
  enemies <- mapM spawnEnemy [1]  -- Create 1 enemy at a time
  return enemies
  where
    spawnEnemy _ = do
      x <- randomRIO (-700.0, 700.0)  -- Random x position within the new screen width range
      let y = 400.0  -- Fixed y position at 400 (can be adjusted as needed)
      enemyPic <- loadBMP "enemies.bmp"  -- Load enemy image
      return $ Enemy (x, y) enemyPic  -- Create and return an enemy

-- Move the bullet upwards
moveBullet :: Bullet -> Bullet
moveBullet (Bullet (x, y)) = Bullet (x, y + 5)  -- Move the bullet upwards by 5 units

-- Update position based on key direction with boundary checks
move :: (Float, Float) -> Char -> (Float, Float)
move (x, y) 'w' = (x, min (y + moveAmount) 420)  -- Move up
move (x, y) 'a' = (max (x - moveAmount) (-720), y)  -- Move left with boundary check
move (x, y) 's' = (x, max (y - moveAmount) (-430))  -- Move down
move (x, y) 'd' = (min (x + moveAmount) 720, y)  -- Move right with boundary check
move pos _      = pos  -- Ignore other keys


-- Handle user input (including mouse clicks)
-- Handle user input (including mouse clicks)
input :: Event -> GameState -> IO GameState
input e gstate = case e of
  -- Handle "Start Game" button click in PreGame
  EventKey (MouseButton LeftButton) Down _ (mx, my) -> 
    if gameMode gstate == PreGame && isTopClicked (mx, my)
    then return gstate { gameMode = InGame }  -- Start the game
    else if gameMode gstate == PreGame && isBottomClicked (mx, my)
      then return gstate { gameMode = ControlsScreen }  -- Show the controls screen
    else if gameMode gstate == GameOver && isTopClicked (mx, my)
      then do
        -- Reset the game state when clicking "Start Over"
        let resetState = resetGameState gstate
        return resetState { gameMode = InGame }
    else if gameMode gstate == ControlsScreen && isBottomClicked (mx, my)
      then return gstate { gameMode = PreGame }  -- Go back to PreGame
    else if gameMode gstate == GameOver && isBottomClicked (mx, my)
      then do
        -- Reset the game state when clicking "Start Over"
        let resetState = resetGameState gstate
        return resetState { gameMode = PreGame }
    else
      return gstate

  -- Handle movement keys ('w', 'a', 's', 'd')
  EventKey (Char c) Down _ _    | c `elem` "wasd" -> return $ gstate { activeKeys = c : activeKeys gstate }
  EventKey (Char c) Up _ _      | c `elem` "wasd" -> return $ gstate { activeKeys = filter (/= c) (activeKeys gstate) }

  -- Handle shooting with "f" key
  EventKey (Char 'f') Down _ _  -> return $ gstate { activeKeys = 'f' : activeKeys gstate }
  EventKey (Char 'f') Up _ _    -> return $ gstate { activeKeys = filter (/= 'f') (activeKeys gstate) }

  -- Handle other events (no change)
  _ -> return gstate



-- Function to check if the controls button was clicked
isBottomClicked :: (Float, Float) -> Bool
isBottomClicked (mx, my) =
  mx >= -130 && mx <= 70 && my >= -170 && my <= -70  -- Adjust button position and size as needed


-- Function to check if the start over button was clicked
isTopClicked :: (Float, Float) -> Bool
isTopClicked (mx, my) =
  mx >= -130 && mx <= 70 && my >= -60 && my <= 40  -- Adjust button position and size as needed




-- Function to reset the game state to the initial state
-- Reset the game state to its initial values
resetGameState :: GameState -> GameState
resetGameState gstate = gstate {
  position = (0, 0),  -- Reset player position
  activeKeys = [],    -- Clear active keys
  bullets = [],       -- Clear bullets
  enemies = [],       -- Clear enemies
  isAlive = True,     -- Reset player's life
  lives = 2,          -- Set the starting number of lives
  elapsedTime = 0,    -- Reset the elapsed time
  cooldownTime = 0,    -- Reset cooldown for shooting
  backgroundPosition = 0

}




-- Path to high scores file
highScoreFile :: FilePath
highScoreFile = "highscores.txt"

-- Function to read high scores from file and print them to the console
readHighScores :: IO [Int]
readHighScores = do
    content <- readFile highScoreFile
    let scores = map read (lines content) :: [Int]
    return scores

-- Function to save a new high score with debug messages
writeHighScore :: Int -> IO ()
writeHighScore newScore = do
    putStrLn "Starting to save a new high score..."

    -- Read existing high scores
    putStrLn "Reading existing high scores..."
    scores <- readHighScores
    putStrLn $ "Current scores: " ++ show scores

    -- Insert the new score in the correct order (descending)
    let updatedScores = insertScore newScore scores
    putStrLn $ "Updated scores (after insertion): " ++ show updatedScores

    -- Write the updated scores to the file
    putStrLn "Writing updated scores to file..."
    writeFile highScoreFile (unlines (map show updatedScores))

-- Helper function to insert a score in descending order
insertScore :: Int -> [Int] -> [Int]
insertScore newScore scores = 
    sortBy (flip compare) (newScore : scores)  -- Sorts in descending order