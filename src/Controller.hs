module Controller where

import Model
import Graphics.Gloss.Interface.IO.Game
import Data.List (sortBy)
import System.IO (readFile, writeFile, appendFile)
import Graphics.Gloss
import System.Random (randomRIO)  

-- player speed
moveAmount :: Float
moveAmount = 2

step :: Float -> GameState -> IO GameState
step secs gstate =
  if paused gstate then 
    return gstate  
  else do
    let newElapsedTime = elapsedTime gstate + secs
        newCooldownTime = max 0 (cooldownTime gstate - secs)
        
    -- scrolling effect for the background
    let newBgPosition = backgroundPosition gstate - (100 * secs)
        newBgPosition2 = backgroundPosition2 gstate - (100 * secs)
        finalBgPosition = if newBgPosition <= -900 then 900 else newBgPosition
        finalBgPosition2 = if newBgPosition2 <= -900 then 900 else newBgPosition2

    -- Update player position
    let (x, y) = position gstate
        newPosition = if isAlive gstate
                      then foldl move (x, y) (activeKeys gstate)
                      else (x, y)

    -- Move existing bullets
    let updatedBullets = filter (\(Bullet (_, by)) -> by <= 900) $ map moveBullet (bullets gstate)

    -- Generate a new bullet
    let newBullet = if 'f' `elem` activeKeys gstate && newCooldownTime == 0 && isAlive gstate
                    then [Bullet (x, y + 20)]
                    else []
        allBullets = newBullet ++ updatedBullets
        finalCooldownTime = if 'f' `elem` activeKeys gstate && newCooldownTime == 0 && isAlive gstate
                            then 0.2
                            else newCooldownTime

    -- 2 enemies on screen
    enemiesToAdd' <- if length (enemies gstate) < 2
                     then do
                       newEnemies <- spawnRandomEnemies 1
                       return (newEnemies ++ enemies gstate)
                     else return (enemies gstate)

    -- Enemy movement logic
    let updatedEnemies = map (`moveEnemy` (x, y)) enemiesToAdd'

    -- Collision bullet enemy
    (enemiesAfterCollisions, newExplosions, enemiesHitCount) <- handleCollisions allBullets updatedEnemies

    -- Explosion animation
    let updatedExplosions = [e { explosionTimeLeft = explosionTimeLeft e - secs } | e <- explosions gstate, explosionTimeLeft e > secs]
        finalExplosions = updatedExplosions ++ newExplosions

    -- Collision player enemy
    let (enemiesAfterPlayerCollisions, playerHitCount, newIsAlive, newGameMode) = handlePlayerCollisions newPosition enemiesAfterCollisions gstate


    -- Player health collision
    let (enemiesAfterPlayerCollisions, newLives, newIsAlive, newGameMode) = handlePlayerCollisions newPosition enemiesAfterCollisions gstate
        updatedScore = score gstate + enemiesHitCount
        
    let newHealth = lives gstate - playerHitCount
        finalHealth = max 0 newHealth

    -- Update score 
    let updatedScore = score gstate + enemiesHitCount
    if newGameMode == GameOver && not (scoreSaved gstate)
      then do
        writeHighScore (score gstate)  
        return gstate {
          gameMode = newGameMode,
          scoreSaved = True         
        }
      else
        return gstate {
          elapsedTime = newElapsedTime,
          position = newPosition,
          bullets = allBullets,
          enemies = enemiesAfterPlayerCollisions,
          cooldownTime = finalCooldownTime,
          backgroundPosition = finalBgPosition,
          backgroundPosition2 = finalBgPosition2,
          score = updatedScore,
          explosions = finalExplosions,
          lives = newLives,
          isAlive = newIsAlive,
          gameMode = newGameMode,
          scoreSaved = scoreSaved gstate  
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


-- Function to handle player collisions with enemies, updating the enemies list and lives
handlePlayerCollisions :: (Float, Float) -> [Enemy] -> GameState -> ([Enemy], Int, Bool, GameMode)
handlePlayerCollisions playerPos enemies gstate =
  let 
      -- Filter out enemies that collide with the player
      remainingEnemies = filter (not . isHitByPlayer playerPos) enemies
      -- Count how many enemies hit the player
      enemiesHitCount = length enemies - length remainingEnemies
      -- Update lives based on the number of collisions
      newLives = lives gstate - enemiesHitCount
      -- Check if player is still alive after losing lives
      newIsAlive = newLives > 0
      -- Set game mode to GameOver if no lives left
      newGameMode = if newLives <= 0 then GameOver else gameMode gstate
  in (remainingEnemies, newLives, newIsAlive, newGameMode)

-- Helper function to check if the player is near an enemy (within a collision radius)
isNearTo :: (Float, Float) -> (Float, Float) -> Bool
isNearTo (px, py) (ex, ey) = (px - ex)^2 + (py - ey)^2 < 25



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

moveEnemy :: Enemy -> (Float, Float) -> Enemy
moveEnemy (Enemy (ex, ey) pic) (px, py) =
  let
    -- If the enemy is higher than the player, move horizontally towards the player
    newX | ey > py && ex < px = ex + 1  -- Move right if the enemy is to the left of the player
         | ey > py && ex > px = ex - 1  -- Move left if the enemy is to the right of the player
         | otherwise = ex  -- No horizontal movement once the enemy is below or aligned with the player

    -- If the enemy is higher than the player, move towards the player (upward)
    -- Otherwise, move downward if the enemy is below the player
    newY | ey > py = ey - 2  -- Move up towards the player if the enemy is above the player
         | otherwise = ey - 2  -- Move down if the enemy is below the player

  in Enemy (newX, newY) pic  -- Return the updated enemy with new position




-- Update position based on key direction with boundary checks
move :: (Float, Float) -> Char -> (Float, Float)
move (x, y) 'w' = (x, min (y + moveAmount) 420)  -- Move up
move (x, y) 'a' = (max (x - moveAmount) (-720), y)  -- Move left with boundary check
move (x, y) 's' = (x, max (y - moveAmount) (-430))  -- Move down
move (x, y) 'd' = (min (x + moveAmount) 720, y)  -- Move right with boundary check
move pos _      = pos  -- Ignore other keys


input :: Event -> GameState -> IO GameState
input e gstate = case e of
  -- Toggle the paused state when 'p' is pressed
  EventKey (Char 'p') Down _ _ -> return gstate { paused = not (paused gstate) }

  -- Handle other input events like movement and shooting
  EventKey (Char c) Down _ _    | c `elem` "wasd" -> return $ gstate { activeKeys = c : activeKeys gstate }
  EventKey (Char c) Up _ _      | c `elem` "wasd" -> return $ gstate { activeKeys = filter (/= c) (activeKeys gstate) }

  -- Handle shooting with "f" key
  EventKey (Char 'f') Down _ _  -> return $ gstate { activeKeys = 'f' : activeKeys gstate }
  EventKey (Char 'f') Up _ _    -> return $ gstate { activeKeys = filter (/= 'f') (activeKeys gstate) }

  -- Handle mouse clicks for game modes
  EventKey (MouseButton LeftButton) Down _ (mx, my) -> 
    if gameMode gstate == PreGame && isTopClicked (mx, my)
    then return gstate { gameMode = InGame }  -- Start the game
    else if gameMode gstate == PreGame && isBottomClicked (mx, my)
      then return gstate { gameMode = ControlsScreen }  -- Show controls screen
    else if gameMode gstate == PreGame && isLowestClicked (mx, my)
      then return gstate { gameMode = BackStory }  -- Show controls screen
    else if gameMode gstate == GameOver && isTopClicked (mx, my)
      then do
        let resetState = resetGameState gstate
        return resetState { gameMode = InGame }
    else if gameMode gstate == ControlsScreen && isBottomClicked (mx, my)
      then return gstate { gameMode = PreGame }
    else if gameMode gstate == BackStory && isLowestLowestClicked (mx, my)
      then return gstate { gameMode = PreGame }
    else if gameMode gstate == GameOver && isBottomClicked (mx, my)
      then do
        let resetState = resetGameState gstate
        return resetState { gameMode = PreGame }
    else
      return gstate

  -- Other event handling...
  _ -> return gstate

-- Function to check if the controls button was clicked
isBottomClicked :: (Float, Float) -> Bool
isBottomClicked (mx, my) =
  mx >= -130 && mx <= 70 && my >= -170 && my <= -70  -- Adjust button position and size as needed

-- Function to check if the controls button was clicked
isLowestClicked :: (Float, Float) -> Bool
isLowestClicked (mx, my) =
  mx >= -130 && mx <= 70 && my >= -280 && my <= -180  -- Adjust button position and size as needed

-- Function to check if the controls button was clicked
isLowestLowestClicked :: (Float, Float) -> Bool
isLowestLowestClicked (mx, my) =
  mx >= -130 && mx <= 70 && my >= -390 && my <= -290  -- Adjust button position and size as needed

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