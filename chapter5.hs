-- HC5T1: Apply function three times
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))

-- HC5T2: Filter odd numbers from 1 to 30
oddNumbers :: [Int]
oddNumbers = filter odd [1..30]

-- HC5T3: Check if any word starts with an uppercase letter
hasUppercaseStart :: [String] -> Bool
hasUppercaseStart = any (\w -> not (null w) && head w `elem` ['A'..'Z'])

-- HC5T4: Lambda function
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

-- HC5T5: Partial application
multiplyByFive :: Int -> Int
multiplyByFive = (* 5)

-- HC5T6: Function composition to square and keep even numbers
squareEvens :: [Int] -> [Int]
squareEvens = filter even . map (^2)

-- HC5T7: Rewrite using $
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

-- HC5T8: Point-free style
addFive :: Int -> Int
addFive = (+ 5)

-- HC5T9: Apply a function twice to every element
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

-- HC5T10: Combine filter, map, any
anySquareGreaterThan50 :: [Int] -> Bool
anySquareGreaterThan50 = any (> 50) . map (^2) . filter (> 0)

-- MAIN
main :: IO ()
main = do
    putStrLn "== HC5T1: applyThrice (*2) 1 =="
    print (applyThrice (*2) 1)  -- 8

    putStrLn "\n== HC5T2: oddNumbers from 1 to 30 =="
    print oddNumbers

    putStrLn "\n== HC5T3: hasUppercaseStart [\"hello\", \"World\", \"apple\"] =="
    print (hasUppercaseStart ["hello", "World", "apple"])

    putStrLn "\n== HC5T4: biggerThan10 using lambda =="
    print (biggerThan10 12)
    print (biggerThan10 8)

    putStrLn "\n== HC5T5: multiplyByFive =="
    print (multiplyByFive 6)

    putStrLn "\n== HC5T6: squareEvens [1..10] =="
    print (squareEvens [1..10])

    putStrLn "\n== HC5T7: result using $ =="
    print result

    putStrLn "\n== HC5T8: addFive point-free =="
    print (addFive 7)

    putStrLn "\n== HC5T9: transformList (*2) [1, 2, 3] =="
    print (transformList (*2) [1, 2, 3])  -- Applies (*2) twice => (*4)

    putStrLn "\n== HC5T10: anySquareGreaterThan50 [3, 6, 8] =="
    print (anySquareGreaterThan50 [3, 6, 8])  -- 64 > 50 → True

    putStrLn "\n== HC5T10: anySquareGreaterThan50 [2, 4, 5] =="
    print (anySquareGreaterThan50 [2, 4, 5])  -- Max is 25 → False
