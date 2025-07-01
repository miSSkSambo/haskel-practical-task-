import Data.List (sortBy)

-- HC1T1 - Task 1: Function Composition
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

-- HC1T2 - Task 2: Pure Function Example
circleArea :: Floating a => a -> a
circleArea r = pi * r^2

-- HC1T3 - Task 3: Checking if a Number is Greater than 18
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

-- HC1T4 - Task 4: Composing a Function to Process Player Data
type Player = (String, Int)

extractPlayers :: [Player] -> [String]
extractPlayers = map fst

sortByScore :: [Player] -> [Player]
sortByScore = reverse . sortBy (\(_, s1) (_, s2) -> compare s1 s2)

topThree :: [Player] -> [Player]
topThree = take 3

getTopThreePlayers :: [Player] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

-- HC1T5 - Task 5: Laziness in Haskell
infiniteNumbers :: [Integer]
infiniteNumbers = [1..]

firstN :: Int -> [Integer]
firstN n = take n infiniteNumbers

-- HC1T6 - Task 6: Using Type Signatures
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- HC1T7 - Task 7: Converting Fahrenheit to Celsius
fToC :: Floating a => a -> a
fToC f = (f - 32) * 5 / 9

-- HC1T8 - Task 8: Higher-Order Functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Main function to demonstrate all tasks
main :: IO ()
main = do
    putStrLn "HC1T1: doubleThenIncrement 4"
    print (doubleThenIncrement 4)

    putStrLn "\nHC1T2: circleArea 5"
    print (circleArea 5)

    putStrLn "\nHC1T3: greaterThan18 20"
    print (greaterThan18 20)

    putStrLn "\nHC1T4: Top 3 Players"
    let players = [("Alice", 50), ("Bob", 70), ("Charlie", 65), ("Daisy", 80)]
    print (getTopThreePlayers players)

    putStrLn "\nHC1T5: First 10 Infinite Numbers"
    print (firstN 10)

    putStrLn "\nHC1T6: addNumbers 7 8"
    print (addNumbers 7 8)

    putStrLn "\nHC1T7: fToC 98.6"
    print (fToC 98.6)

    putStrLn "\nHC1T8: applyTwice (*2) 3"
    print (applyTwice (*2) 3)
