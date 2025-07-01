-- HC6T1: Recursive factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- HC6T2: Recursive Fibonacci
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- HC6T3: Sum of list using foldr
sumList :: [Int] -> Int
sumList = foldr (+) 0

-- HC6T4: Product of list using foldl
productList :: [Int] -> Int
productList = foldl (*) 1

-- HC6T5: Reverse a list using recursion
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- HC6T6: Check if element exists in list
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists y (x:xs)
    | y == x    = True
    | otherwise = elementExists y xs

-- HC6T7: Length of a list
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

-- HC6T8: Filter even numbers
filterEvens :: [Int] -> [Int]
filterEvens [] = []
filterEvens (x:xs)
    | even x    = x : filterEvens xs
    | otherwise = filterEvens xs

-- HC6T9: Map function (custom implementation)
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- HC6T10: Digits of a number (recursive)
digits :: Int -> [Int]
digits n
    | n < 10    = [n]
    | otherwise = digits (n `div` 10) ++ [n `mod` 10]

-- MAIN
main :: IO ()
main = do
    putStrLn "== HC6T1: factorial 5 =="
    print (factorial 5)

    putStrLn "\n== HC6T2: fibonacci 7 =="
    print (fibonacci 7)

    putStrLn "\n== HC6T3: sumList [1,2,3,4,5] =="
    print (sumList [1,2,3,4,5])

    putStrLn "\n== HC6T4: productList [1,2,3,4,5] =="
    print (productList [1,2,3,4,5])

    putStrLn "\n== HC6T5: reverseList [1,2,3,4] =="
    print (reverseList [1,2,3,4])

    putStrLn "\n== HC6T6: elementExists 3 [1,2,3,4] =="
    print (elementExists 3 [1,2,3,4])

    putStrLn "\n== HC6T7: listLength [10,20,30,40] =="
    print (listLength [10,20,30,40])

    putStrLn "\n== HC6T8: filterEvens [1..10] =="
    print (filterEvens [1..10])

    putStrLn "\n== HC6T9: myMap (+1) [1,2,3] =="
    print (myMap (+1) [1,2,3])

    putStrLn "\n== HC6T10: digits 12345 =="
    print (digits 12345)
