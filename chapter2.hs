-- HC2T2 - Task 2: Function Type Signatures and Implementation
add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2

-- HC2T3 - Task 3: Immutable Variables
myAge :: Int
myAge = 22

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

-- HC2T4 - Task 4: Infix <-> Prefix
-- Prefix notation of:
prefix1 = (+) 5 3
prefix2 = (*) 10 4
prefix3 = (&&) True False

-- Infix notation of:
infix1 = 7 + 2
infix2 = 6 * 5
infix3 = True && False

-- HC2T5 - Task 5: Functions
circleArea :: Float -> Float
circleArea r = pi * r * r

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z = max x (max y z)

-- HC2T6 - Task 6: Int vs Integer
smallNumber :: Int
smallNumber = 2^62

bigNumber :: Integer
bigNumber = 2^127

-- HC2T7 - Task 7: Boolean Expressions
bool1 = True && True        -- True
bool2 = False || False      -- False
bool3 = not False           -- True
bool4 = 10 > 100            -- False

-- HC2T1 - Task 1: Types (Expected types)
-- 42           :: Int
-- 3.14         :: Fractional a => a (commonly Double)
-- "Haskell"    :: String
-- 'Z'          :: Char
-- True && False:: Bool

-- Main to demonstrate all tasks
main :: IO ()
main = do
    putStrLn "== HC2T1: Types (Expected in GHCi) =="
    putStrLn "42 :: Int"
    putStrLn "3.14 :: Fractional a => a"
    putStrLn "\"Haskell\" :: String"
    putStrLn "'Z' :: Char"
    putStrLn "True && False :: Bool"

    putStrLn "\n== HC2T2: Functions =="
    print (add 5 6)
    print (isEven 4)
    print (concatStrings "Hello, " "World!")

    putStrLn "\n== HC2T3: Immutable Variables =="
    print myAge
    print piValue
    print greeting
    print isHaskellFun
    -- Uncommenting this would cause a compile-time error
    -- myAge = 30

    putStrLn "\n== HC2T4: Infix <-> Prefix =="
    print prefix1
    print prefix2
    print prefix3
    print infix1
    print infix2
    print infix3

    putStrLn "\n== HC2T5: More Functions =="
    print (circleArea 5)
    print (maxOfThree 8 3 6)

    putStrLn "\n== HC2T6: Int vs Integer =="
    print smallNumber
    print bigNumber
    -- 2^64 :: Int -- Try this in GHCi: will likely overflow

    putStrLn "\n== HC2T7: Boolean Expressions =="
    print bool1
    print bool2
    print bool3
    print bool4
