-- HC4T1: weatherReport using pattern matching
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

-- HC4T2: dayType using pattern matching
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType day
  | day `elem` ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"] = "It's a weekday."
  | otherwise = "Invalid day"

-- HC4T3: gradeComment using guards
gradeComment :: Int -> String
gradeComment n
  | n >= 90 && n <= 100 = "Excellent!"
  | n >= 70 && n < 90   = "Good job!"
  | n >= 50 && n < 70   = "You passed."
  | n >= 0  && n < 50   = "Better luck next time."
  | otherwise           = "Invalid grade"

-- HC4T4 & HC4T5: specialBirthday with pattern matching and catch-all
specialBirthday :: Int -> String
specialBirthday 1  = "First birthday! How cute!"
specialBirthday 18 = "You're an adult now!"
specialBirthday 21 = "Time to party! 🎉"
specialBirthday age = "Happy Birthday! You're " ++ show age ++ " years old."

-- HC4T6: whatsInsideThisList
whatsInsideThisList :: [a] -> String
whatsInsideThisList []       = "The list is empty."
whatsInsideThisList [_]      = "The list has one item."
whatsInsideThisList [_, _]   = "The list has two items."
whatsInsideThisList _        = "The list has many items."

-- HC4T7: firstAndThird
firstAndThird :: [a] -> [a]
firstAndThird (x:_:z:_) = [x, z]
firstAndThird _         = []

-- HC4T8: describeTuple
describeTuple :: (String, Int, Bool) -> String
describeTuple (name, age, status) =
    name ++ " is " ++ show age ++ " years old and it is " ++ show status ++ " that they are active."

-- MAIN
main :: IO ()
main = do
    putStrLn "== HC4T1: weatherReport =="
    print (weatherReport "sunny")
    print (weatherReport "rainy")
    print (weatherReport "windy")

    putStrLn "\n== HC4T2: dayType =="
    print (dayType "Saturday")
    print (dayType "Monday")
    print (dayType "Funday")

    putStrLn "\n== HC4T3: gradeComment =="
    print (gradeComment 95)
    print (gradeComment 75)
    print (gradeComment 55)
    print (gradeComment 30)
    print (gradeComment 110)

    putStrLn "\n== HC4T4 & HC4T5: specialBirthday =="
    print (specialBirthday 1)
    print (specialBirthday 18)
    print (specialBirthday 21)
    print (specialBirthday 33)

    putStrLn "\n== HC4T6: whatsInsideThisList =="
    print (whatsInsideThisList ([] :: [Int]))
    print (whatsInsideThisList [42])
    print (whatsInsideThisList [1, 2])
    print (whatsInsideThisList [1, 2, 3, 4])

    putStrLn "\n== HC4T7: firstAndThird =="
    print (firstAndThird [10, 20, 30, 40])
    print (firstAndThird [1, 2])
    print (firstAndThird [5, 6, 7])

    putStrLn "\n== HC4T8: describeTuple =="
    print (describeTuple ("Alice", 30, True))
    print (describeTuple ("Bob", 22, False))
