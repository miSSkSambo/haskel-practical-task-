import Data.Char (toUpper)
import qualified Data.List as L

-- HC16T1: Reverse a String
reverseStr :: String -> String
reverseStr = reverse

-- HC16T2: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome s = let cleaned = map toUpper s
                 in cleaned == reverse cleaned

-- HC16T3: Factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- HC16T4: Filter Even Numbers
filterEven :: [Int] -> [Int]
filterEven = filter even

-- HC16T5: Uppercase String
toUpperStr :: String -> String
toUpperStr = map toUpper

-- HC16T6: nth Fibonacci Number
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- HC16T7: Element Existence in List
exists :: Eq a => a -> [a] -> Bool
exists = elem

-- HC16T8: Insertion Sort
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert x ys

insertionSort :: [Int] -> [Int]
insertionSort []     = []
insertionSort (x:xs) = insert x (insertionSort xs)

-- HC16T9: Remove Duplicates from List
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

-- HC16T10: Character Frequency in String
charFrequency :: String -> [(Char, Int)]
charFrequency s = [(c, count c s) | c <- L.nub s]
  where count ch = length . filter (== ch)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "=== HC16T1: Reverse a String ==="
  print (reverseStr "haskell")

  putStrLn "\n=== HC16T2: Palindrome Checker ==="
  print (isPalindrome "madam")
  print (isPalindrome "hello")

  putStrLn "\n=== HC16T3: Factorial ==="
  print (factorial 5)

  putStrLn "\n=== HC16T4: Filter Even Numbers ==="
  print (filterEven [1..10])

  putStrLn "\n=== HC16T5: Uppercase String ==="
  print (toUpperStr "functional programming")

  putStrLn "\n=== HC16T6: nth Fibonacci Number ==="
  print (fib 10)

  putStrLn "\n=== HC16T7: Element Existence in List ==="
  print (exists 3 [1,2,3,4,5])
  print (exists 9 [1,2,3,4,5])

  putStrLn "\n=== HC16T8: Insertion Sort ==="
  print (insertionSort [5,3,8,1,2])

  putStrLn "\n=== HC16T9: Remove Duplicates from List ==="
  print (removeDuplicates [1,2,2,3,1,4,3,5])

  putStrLn "\n=== HC16T10: Character Frequency in String ==="
  print (charFrequency "mississippi")
