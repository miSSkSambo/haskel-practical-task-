import Text.Printf (printf)

-- HC3T1: Check if number is positive, negative, or zero
checkNumber :: Int -> String
checkNumber x =
    if x > 0 then "Positive"
    else if x < 0 then "Negative"
    else "Zero"

-- HC3T2: Determine grade using guards
grade :: Int -> String
grade x
    | x >= 90 = "A"
    | x >= 80 = "B"
    | x >= 70 = "C"
    | x >= 60 = "D"
    | otherwise = "F"

-- HC3T3: Convert RGB to Hex
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) =
    let rHex = printf "%02X" r
        gHex = printf "%02X" g
        bHex = printf "%02X" b
    in rHex ++ gHex ++ bHex

-- HC3T4: Triangle area using Heron's formula
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
    let s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c))

-- HC3T5: Triangle type
triangleType :: Float -> Float -> Float -> String
triangleType a b c
    | a == b && b == c = "Equilateral"
    | a == b || b == c || a == c = "Isosceles"
    | otherwise = "Scalene"

-- HC3T6: Check leap year
isLeapYear :: Int -> Bool
isLeapYear year =
    if year `mod` 400 == 0 then True
    else if year `mod` 100 == 0 then False
    else if year `mod` 4 == 0 then True
    else False

-- HC3T7: Season based on month
season :: Int -> String
season month
    | month == 12 || month == 1 || month == 2 = "Winter"
    | month >= 3 && month <= 5 = "Spring"
    | month >= 6 && month <= 8 = "Summer"
    | month >= 9 && month <= 11 = "Autumn"
    | otherwise = "Invalid month"

-- HC3T8: BMI category
bmiCategory :: Float -> Float -> String
bmiCategory weight height
    | bmi < 18.5 = "Underweight"
    | bmi < 25 = "Normal"
    | bmi < 30 = "Overweight"
    | otherwise = "Obese"
    where bmi = weight / height^2

-- HC3T9: Max of three using let
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z =
    let maxXY = max x y
        maxAll = max maxXY z
    in maxAll

-- HC3T10: Palindrome check
isPalindrome :: String -> Bool
isPalindrome s
    | length s <= 1 = True
    | head s == last s = isPalindrome (init (tail s))
    | otherwise = False

-- Main Function
main :: IO ()
main = do
    putStrLn "== HC3T1: checkNumber =="
    print (checkNumber 5)
    print (checkNumber (-3))
    print (checkNumber 0)

    putStrLn "\n== HC3T2: grade =="
    print (grade 95)
    print (grade 72)
    print (grade 50)

    putStrLn "\n== HC3T3: rgbToHex =="
    print (rgbToHex (255, 0, 127))
    print (rgbToHex (0, 255, 64))

    putStrLn "\n== HC3T4: triangleArea =="
    print (triangleArea 3 4 5)
    print (triangleArea 7 8 9)

    putStrLn "\n== HC3T5: triangleType =="
    print (triangleType 3 3 3)
    print (triangleType 5 5 8)
    print (triangleType 6 7 8)

    putStrLn "\n== HC3T6: isLeapYear =="
    print (isLeapYear 2000)
    print (isLeapYear 1900)
    print (isLeapYear 2024)

    putStrLn "\n== HC3T7: season =="
    print (season 3)
    print (season 7)
    print (season 11)

    putStrLn "\n== HC3T8: bmiCategory =="
    print (bmiCategory 70 1.75)
    print (bmiCategory 90 1.8)

    putStrLn "\n== HC3T9: maxOfThree =="
    print (maxOfThree 10 20 15)
    print (maxOfThree 5 25 10)

    putStrLn "\n== HC3T10: isPalindrome =="
    print (isPalindrome "racecar")
    print (isPalindrome "haskell")
    print (isPalindrome "madam")
