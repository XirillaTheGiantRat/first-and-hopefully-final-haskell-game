module Controller where

import Model
import Graphics.Gloss.Interface.IO.Game
import System.IO (readFile)

-- Movement amount per frame
moveAmount :: Float
moveAmount = 2

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = 
  let (x, y) = position gstate
      -- Calculate new position based on active keys
      newPosition = foldl move (x, y) (activeKeys gstate)
      -- Update positions of bullets and remove those off-screen (e.g., bullets with y > 900)
      updatedBullets = filter (\(Bullet (_, by)) -> by <= 900) $ map moveBullet (bullets gstate)
  in return $ gstate { elapsedTime = elapsedTime gstate + secs, position = newPosition, bullets = updatedBullets }


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

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'f') Down _ _) gstate = 
  -- When spacebar is pressed, create a new bullet and add it to the bullets list
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
    putStrLn "High Scores (Debug):"
    mapM_ print scores  -- Print each score to the console
    return scores