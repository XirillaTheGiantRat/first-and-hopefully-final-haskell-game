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
  -- Calculate the new elapsed time and background scrolling
  let newElapsedTime = elapsedTime gstate + secs
      newBgPosition = backgroundPosition gstate - (100 * secs)
      newBgPosition2 = backgroundPosition2 gstate - (100 * secs)
      finalBgPosition = if newBgPosition <= -900 then 900 else newBgPosition
      finalBgPosition2 = if newBgPosition2 <= -900 then 900 else newBgPosition2
      newCooldownTime = max 0 (cooldownTime gstate - secs)

  -- Ensure two enemies are always present
  enemiesToAdd' <- if length (enemies gstate) < 2
                   then do
                     newEnemies <- spawnRandomEnemies 1
                     return (newEnemies ++ enemies gstate)
                   else return (enemies gstate)

  -- Move all enemies upwards
  let updatedEnemies = map moveEnemy enemiesToAdd'  -- Move all enemies upwards

  -- Player movement and bullet handling
  let (x, y) = position gstate
      newPosition = if isAlive gstate
                    then foldl move (x, y) (activeKeys gstate)
                    else (x, y)
      updatedBullets = filter (\(Bullet (_, by)) -> by <= 900) $ map moveBullet (bullets gstate)
      newBullet = if 'f' `elem` activeKeys gstate && newCooldownTime == 0 && isAlive gstate
                  then [Bullet (x, y + 20)]
                  else []
      allBullets = newBullet ++ updatedBullets
      finalCooldownTime = if 'f' `elem` activeKeys gstate && newCooldownTime == 0 && isAlive gstate
                           then 0.2
                           else newCooldownTime


  -- Handle collisions and add explosions
  (enemiesAfterCollisions, newExplosions, enemiesHitCount) <- handleCollisions allBullets updatedEnemies  -- Updated to use `updatedEnemies`

  -- Update explosions, reducing their time left and removing those that are finished
  let updatedExplosions = [e { explosionTimeLeft = explosionTimeLeft e - secs } | e <- explosions gstate, explosionTimeLeft e > secs]

  -- Combine the new and updated explosions
  let finalExplosions = updatedExplosions ++ newExplosions

  -- Handle player collisions with enemies, removing only those that collide without affecting health
  let enemiesAfterPlayerCollisions = handlePlayerCollisions newPosition enemiesAfterCollisions

  -- Update the score based on the number of enemies hit by bullets
  let updatedScore = score gstate + enemiesHitCount

  -- Return the updated game state with the new values
  if gameMode gstate == GameOver && gameMode gstate /= GameOver
    then do
      endGame gstate  -- Save the score to the high scores file
      return gstate
    else
      return gstate {
        elapsedTime = newElapsedTime,
        position = newPosition,
        bullets = allBullets,
        enemies = enemiesAfterPlayerCollisions,  -- Use the updated enemies here
        cooldownTime = finalCooldownTime,
        backgroundPosition = finalBgPosition,
        backgroundPosition2 = finalBgPosition2,
        score = updatedScore,
        explosions = finalExplosions  -- Update explosions in game state
      }


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
handleCollisions :: [Bullet] -> [Enemy] -> IO ([Enemy], [Explosion], Int)
handleCollisions bullets enemies = do
    let hitEnemies = filter (isHitByBullet bullets) enemies
        remainingEnemies = filter (not . isHitByBullet bullets) enemies
        enemiesHitCount = length hitEnemies
        explosions = [Explosion (ex, ey) 0.5 | Enemy (ex, ey) _ <- hitEnemies]  -- Create explosions at hit positions

    if length remainingEnemies < 2
      then do
        newEnemies <- spawnRandomEnemies 1
        return (remainingEnemies ++ newEnemies, explosions, enemiesHitCount)
      else return (remainingEnemies, explosions, enemiesHitCount)



-- Helper function to partition enemies into hit and remaining ones
partitionEnemies :: [Bullet] -> [Enemy] -> ([Enemy], [Enemy])
partitionEnemies bullets enemies =
  foldr (\enemy (remaining, hit) ->
            if isHitByBullet bullets enemy
            then (remaining, enemy : hit)
            else (enemy : remaining, hit))
        ([], []) enemies

-- Function to handle player collisions with enemies, updating the enemies list
-- Only removes enemies that collide with the player without affecting player health
handlePlayerCollisions :: (Float, Float) -> [Enemy] -> [Enemy]
handlePlayerCollisions playerPos = filter (not . isHitByPlayer playerPos)

-- Function to check if an enemy is hit by any bullet
isHitByBullet :: [Bullet] -> Enemy -> Bool
isHitByBullet bullets enemy = any (isBulletHit enemy) bullets

-- Function to check if a specific bullet hits an enemy
isBulletHit :: Enemy -> Bullet -> Bool
isBulletHit (Enemy (ex, ey) _) (Bullet (bx, by)) =
  -- Check if the bullet's position is close enough to the enemy's position
  abs (bx - ex) < 20 && abs (by - ey) < 20  -- Adjust 20 to the appropriate "hit radius"

-- Function to check for collisions between player and enemies
handleEnemyCollisions :: (Float, Float) -> [Enemy] -> GameState -> GameState
handleEnemyCollisions (px, py) enemies gstate = 
  let remainingEnemies = filter (not . isHitByPlayer (px, py)) enemies
      enemiesHitCount = length enemies - length remainingEnemies
      newLives = if enemiesHitCount > 0 then lives gstate - 1 else lives gstate
      newIsAlive = newLives > 0
      newGameMode = if newLives <= 0 then GameOver else gameMode gstate
  in gstate { enemies = remainingEnemies, lives = newLives, isAlive = newIsAlive, gameMode = newGameMode }

-- Function to check if an enemy is hit by player or has passed the bottom of the screen
isHitByPlayer :: (Float, Float) -> Enemy -> Bool
isHitByPlayer (px, py) (Enemy (ex, ey) _) =
  -- Check if the player is near the enemy (within the "hit radius")
  (abs (px - ex) < 20 && abs (py - ey) < 20) || 
  -- Check if the enemy has reached y = -450, indicating it has passed the bottom of the screen
  ey <= -450


-- Function to check if a specific enemy collides with the player
isPlayerHit :: Enemy -> (Float, Float) -> Bool
isPlayerHit (Enemy (ex, ey) _) (px, py) =
  abs (px - ex) < 20 && abs (py - ey) < 20  -- Adjust the "hit radius" as needed

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

-- Move the enemy upwards
moveEnemy :: Enemy -> Enemy
moveEnemy (Enemy (x, y) pic) = Enemy (x, y - 2) pic  -- Move the enemy upwards by 2 units

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
  backgroundPosition = 0,
  backgroundPosition2 = 900


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


endGame :: GameState -> IO ()
endGame gstate = do
  let finalScore = score gstate
  putStrLn $ "Game Over! Your score was: " ++ show finalScore
  writeHighScore finalScore  -- Save the final score to highscores.txt