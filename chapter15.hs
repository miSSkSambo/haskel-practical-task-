{-# LANGUAGE ScopedTypeVariables #-}

-- Chapter 15: Exception and Error Handling (Tasks 1–10, one by one)
-- No external packages required.

import qualified Control.Exception as E
import           Control.Exception (Exception, IOException, try, catch, throwIO)
import           Data.Typeable     (Typeable)
import           Data.Char         (toLower)
import           Text.Read         (readMaybe)

--------------------------------------------------------------------------------
-- Shared helpers (used by multiple tasks)

-- HC15T5: Safe Division Using Maybe
safeDivMaybe :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDivMaybe _ 0 = Nothing
safeDivMaybe x y = Just (x / y)

-- HC15T6: Safe Input Parsing with readMaybe
parseDouble :: String -> Maybe Double
parseDouble = readMaybe

-- HC15T7: Velocity using Maybe + safe parsing
velocityMaybe :: String -> String -> Maybe Double
velocityMaybe sDist sTime = do
  d <- parseDouble sDist
  t <- parseDouble sTime
  safeDivMaybe d t

-- HC15T8: Division with Either for detailed errors
divEither :: (Eq a, Fractional a, Show a) => a -> a -> Either String a
divEither _ 0 = Left "Divide-by-zero: denominator was 0."
divEither x y = Right (x / y)

-- HC15T3/4: Custom traffic light exception + utils
data TrafficLightError = BadLight String
  deriving (Show, Typeable)
instance Exception TrafficLightError

reactToLight :: String -> IO String
reactToLight color =
  case map toLower color of
    "red"    -> pure "STOP"
    "yellow" -> pure "SLOW"
    "green"  -> pure "GO"
    bad      -> throwIO (BadLight ("Unknown light color: " ++ bad))

handleTraffic :: TrafficLightError -> IO String
handleTraffic (BadLight msg) =
  pure ("Handled TrafficLightError: " ++ msg ++ " -> default = SAFE-STOP")

--------------------------------------------------------------------------------
-- HC15T1: Handle Exceptions for File Reading and Velocity Calculation
task1 :: IO ()
task1 = do
  putStrLn "=== HC15T1 ==="
  -- File reading with exceptions (likely missing to demonstrate handling)
  eContent <- try @IOException (readFile "input.txt")
  case eContent of
    Left err -> putStrLn $ "File read failed: " ++ show err
    Right s  -> putStrLn $ "File read OK, length = " ++ show (length s)

  -- Velocity from “user input” (simulated here with literals), with safe checks
  let userDist = "100"  -- pretend from getLine
      userTime = "20"
  case (parseDouble userDist, parseDouble userTime) of
    (Nothing, _)      -> putStrLn "Velocity error: distance not a number."
    (_, Nothing)      -> putStrLn "Velocity error: time not a number."
    (Just _, Just 0)  -> putStrLn "Velocity error: time cannot be zero."
    (Just d, Just t)  -> putStrLn $ "Velocity = " ++ show (d / t)

--------------------------------------------------------------------------------
-- HC15T2: Self-Driving AI Car System (react to traffic light colors)
task2 :: IO ()
task2 = do
  putStrLn "=== HC15T2 ==="
  actR <- reactToLight "red"
  actY <- reactToLight "Yellow"
  actG <- reactToLight "GREEN"
  putStrLn $ "red    -> "   ++ actR
  putStrLn $ "Yellow -> "   ++ actY
  putStrLn $ "GREEN  -> "   ++ actG

--------------------------------------------------------------------------------
-- HC15T3: Custom Exception for Traffic Light Errors (defined above)
task3 :: IO ()
task3 = do
  putStrLn "=== HC15T3 ==="
  -- Intentionally trigger the custom exception:
  (reactToLight "blue" >> putStrLn "This won't print")
    `catch` \(e :: TrafficLightError) ->
      putStrLn ("Caught custom exception: " ++ show e)

--------------------------------------------------------------------------------
-- HC15T4: Exception Handler for Traffic Light
task4 :: IO ()
task4 = do
  putStrLn "=== HC15T4 ==="
  action <- reactToLight "purple" `catch` handleTraffic
  putStrLn $ "purple -> " ++ action

--------------------------------------------------------------------------------
-- HC15T5: Safe Division Using Maybe
task5 :: IO ()
task5 = do
  putStrLn "=== HC15T5 ==="
  print (safeDivMaybe 10.0 2.0)  -- Just 5.0
  print (safeDivMaybe 10.0 0.0)  -- Nothing

--------------------------------------------------------------------------------
-- HC15T6: Safe Input Parsing with readMaybe
task6 :: IO ()
task6 = do
  putStrLn "=== HC15T6 ==="
  print (parseDouble "12.34")   -- Just 12.34
  print (parseDouble "12.34x")  -- Nothing

--------------------------------------------------------------------------------
-- HC15T7: Velocity Calculation with Optionals and Parsing Handling
task7 :: IO ()
task7 = do
  putStrLn "=== HC15T7 ==="
  print (velocityMaybe "150" "3")   -- Just 50.0
  print (velocityMaybe "150" "0")   -- Nothing
  print (velocityMaybe "x150" "3")  -- Nothing

--------------------------------------------------------------------------------
-- HC15T8: Division with Either for Detailed Errors
task8 :: IO ()
task8 = do
  putStrLn "=== HC15T8 ==="
  print (divEither  9.0 3.0)   -- Right 3.0
  print (divEither  9.0 0.0)   -- Left ...

--------------------------------------------------------------------------------
-- HC15T9: Try Function for File IO Exceptions
task9 :: IO ()
task9 = do
  putStrLn "=== HC15T9 ==="
  res <- try @IOException (readFile "missing.txt")
  case res of
    Left err -> putStrLn $ "try caught IOException: " ++ show err
    Right s  -> putStrLn $ "Read " ++ show (length s) ++ " chars"

--------------------------------------------------------------------------------
-- HC15T10: Hybrid Error Handling with Either and IO
task10 :: IO ()
task10 = do
  putStrLn "=== HC15T10 ==="
  let sDist = "100"
      sTime = "5"
      scaleFile = "scale.txt"   -- if missing or unreadable, we ignore scaling
  case (readMaybe sDist :: Maybe Double, readMaybe sTime :: Maybe Double) of
    (Nothing, _) -> putStrLn "Hybrid error: distance parse failed"
    (_, Nothing) -> putStrLn "Hybrid error: time parse failed"
    (Just d, Just t) ->
      case divEither d t of
        Left msg -> putStrLn $ "Hybrid Either error: " ++ msg
        Right v  -> do
          eScale <- try @IOException (readFile scaleFile)
          case eScale of
            Left _ -> putStrLn $ "Velocity (no scale) = " ++ show v
            Right s ->
              case readMaybe s :: Maybe Double of
                Nothing    -> putStrLn $ "Velocity (bad scale ignored) = " ++ show v
                Just scale -> putStrLn $ "Velocity (scaled) = " ++ show (v * scale)

--------------------------------------------------------------------------------
-- Run tasks in order
main :: IO ()
main = do
  task1
  task2
  task3
  task4
  task5
  task6
  task7
  task8
  task9
  task10
