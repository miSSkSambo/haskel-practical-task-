{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE PartialTypeSignatures #-}

import qualified Data.List as L

-- HC14T5: Custom data type
data Result a = Ok a | Err String deriving Show

-- HC14T8: Character frequency
counts :: String -> [(Char, Int)]
counts str =
  [ (c, length (filter (== c) str))
  | c <- L.nub (L.sort str)
  ]

-- HC14T9: PartialTypeSignatures
safeHead :: [a] -> _
safeHead []    = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
  putStrLn "=== HC14T1: Hello Cabal ==="
  putStrLn "Hello, Cabal!"

  putStrLn "\n=== HC14T2: Simulated Random Number ==="
  let fakeRandom = 42
  putStrLn $ "Random number (simulated) = " ++ show fakeRandom

  putStrLn "\n=== HC14T3: NumericUnderscores ==="
  print (1_000_000 :: Int)

  putStrLn "\n=== HC14T4: TypeApplications ==="
  print (read @Int "123")

  putStrLn "\n=== HC14T5: Result Data Type with Pattern Matching ==="
  print (Ok 99)                        -- Result Int
  print (Err "oops" :: Result Int)     -- add concrete type to avoid ambiguity

  putStrLn "\n=== HC14T8: Character Frequency ==="
  print (counts "mississippi")

  putStrLn "\n=== HC14T9: PartialTypeSignatures ==="
  print (safeHead [10,20,30 :: Int])

  putStrLn "\n=== HC14T10: Test Suite Simulation ==="
  putStrLn "Would check that counts \"mississippi\" == [('i',4),('m',1),('p',2),('s',4)]"
