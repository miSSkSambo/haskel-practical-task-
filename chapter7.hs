import Text.Read (readMaybe)

-- HC7T1: Define Color with Eq instance
data Color = Red | Green | Blue deriving (Show, Read, Enum, Bounded)

instance Eq Color where
    Red == Red = True
    Green == Green = True
    Blue == Blue = True
    _ == _ = False

-- HC7T2: Implement Ord for Color (Red < Green < Blue)
instance Ord Color where
    compare Red Green = LT
    compare Red Blue  = LT
    compare Green Blue = LT
    compare x y
        | x == y    = EQ
        | otherwise = GT

-- HC7T3: compareValues with Eq and Ord
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y = if x >= y then x else y

-- HC7T4: Shape with Show and Read
data Shape = Circle Double | Rectangle Double Double deriving (Eq)

instance Show Shape where
    show (Circle r) = "Circle " ++ show r
    show (Rectangle w h) = "Rectangle " ++ show w ++ " " ++ show h

instance Read Shape where
    readsPrec _ input =
        case words input of
            ["Circle", r] -> [(Circle (read r), "")]
            ["Rectangle", w, h] -> [(Rectangle (read w) (read h), "")]
            _ -> []

-- Added: Ord instance for Shape (based on area)
instance Ord Shape where
    compare s1 s2 = compare (area s1) (area s2)

area :: Shape -> Double
area (Circle r) = pi * r^2
area (Rectangle w h) = w * h

-- HC7T5: squareArea with Num
squareArea :: Num a => a -> a
squareArea s = s * s

-- HC7T6: circleCircumference using Floating
circleCircumference :: (Floating a) => a -> a
circleCircumference r = 2 * pi * r

-- HC7T7: nextColor using Bounded and Enum
nextColor :: Color -> Color
nextColor c
    | c == maxBound = minBound
    | otherwise = succ c

-- HC7T8: parseShape using Read
parseShape :: String -> Maybe Shape
parseShape str = readMaybe str :: Maybe Shape

-- HC7T9: Describable class
class Describable a where
    describe :: a -> String

instance Describable Bool where
    describe True = "It's true!"
    describe False = "It's false!"

instance Describable Shape where
    describe (Circle r) = "A circle with radius " ++ show r
    describe (Rectangle w h) = "A rectangle " ++ show w ++ " by " ++ show h

-- HC7T10: describeAndCompare
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y = describe (compareValues x y)

-- MAIN
main :: IO ()
main = do
    putStrLn "== HC7T1: Eq instance =="
    print (Red == Green)
    print (Red == Red)

    putStrLn "\n== HC7T2: Ord instance =="
    print (Red < Green)
    print (Blue > Green)

    putStrLn "\n== HC7T3: compareValues 3 5 =="
    print (compareValues 3 5)

    putStrLn "\n== HC7T4: Show and Read for Shape =="
    print (show (Rectangle 4.0 5.0))
    print (read "Circle 3.5" :: Shape)

    putStrLn "\n== HC7T5: squareArea 6 =="
    print (squareArea 6)

    putStrLn "\n== HC7T6: circleCircumference 7.5 =="
    print (circleCircumference 7.5)

    putStrLn "\n== HC7T7: nextColor Blue (should wrap to Red) =="
    print (nextColor Blue)
    print (nextColor Green)

    putStrLn "\n== HC7T8: parseShape \"Rectangle 4.0 5.0\" =="
    print (parseShape "Rectangle 4.0 5.0")
    print (parseShape "Triangle 2.0 3.0")

    putStrLn "\n== HC7T9: Describable instances =="
    print (describe True)
    print (describe (Circle 3.0))

    putStrLn "\n== HC7T10: describeAndCompare (Rectangle 3 4) (Rectangle 5 6) =="
    print (describeAndCompare (Rectangle 3 4) (Rectangle 5 6))
