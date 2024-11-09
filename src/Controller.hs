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

-- Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = do
  -- Calculate the new elapsed time
  let newElapsedTime = elapsedTime gstate + secs

  -- Ensure there are always two enemies
  let enemiesToAdd = if length (enemies gstate) < 2
                     then spawnRandomEnemies 1  -- Spawn 1 new enemy if fewer than 2 exist
                     else return (enemies gstate)  -- Keep the existing enemies

  -- Calculate new position based on active keys
  let (x, y) = position gstate
      newPosition = foldl move (x, y) (activeKeys gstate)

  -- Move the bullets and remove those that go off screen
  let updatedBullets = filter (\(Bullet (_, by)) -> by <= 900) $ map moveBullet (bullets gstate)

  -- Get the updated enemies, filtering out those hit by bullets
  enemiesToAdd' <- enemiesToAdd
  updatedEnemies <- handleCollisions (bullets gstate) enemiesToAdd'

  -- Return the updated game state with the new enemies, bullets, and other changes
  return gstate { elapsedTime = newElapsedTime, position = newPosition, bullets = updatedBullets, enemies = updatedEnemies }

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
      x <- randomRIO (-450.0, 450.0)  -- Random x position within the new screen width range
      let y = 400.0  -- Fixed y position at 400 (can be adjusted as needed)
      enemyPic <- loadBMP "enemies.bmp"  -- Load enemy image
      return $ Enemy (x, y) enemyPic  -- Create and return an enemy

-- Move the bullet upwards
moveBullet :: Bullet -> Bullet
moveBullet (Bullet (x, y)) = Bullet (x, y + 5)  -- Move the bullet upwards by 5 units

-- Update position based on key direction
move :: (Float, Float) -> Char -> (Float, Float)
move (x, y) 'w' = (x, y + moveAmount)  -- Move up
move (x, y) 'a' = (x - moveAmount, y)  -- Move left
move (x, y) 's' = (x, y - moveAmount)  -- Move down
move (x, y) 'd' = (x + moveAmount, y)  -- Move right
move pos _      = pos                  -- Ignore other keys

-- Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'f') Down _ _) gstate = 
  let (x, y) = position gstate
      newBullet = Bullet (x, y + 20)  -- Bullet starts just above the spaceship (adjust as needed)
  in gstate { bullets = newBullet : bullets gstate }  -- Add new bullet to the bullets list

inputKey (EventKey (Char c) Down _ _) gstate
  | c `elem` "wasd" = gstate { activeKeys = c : activeKeys gstate }  -- Add key to activeKeys on press
inputKey (EventKey (Char c) Up _ _) gstate
  | c `elem` "wasd" = gstate { activeKeys = filter (/= c) (activeKeys gstate) }  -- Remove key on release
inputKey _ gstate = gstate  -- Keep state unchanged for other events

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