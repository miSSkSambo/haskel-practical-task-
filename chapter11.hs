import Data.Char (toUpper)

main :: IO ()
main = do
    -- HC11T1
    let name = "Katlego Sambo"
    putStrLn $ "Hello, " ++ name ++ "!"

    -- HC11T2
    let line = "Hello world!"
    putStrLn $ "Number of characters: " ++ show (length line)

    -- HC11T3
    let num = 22
    putStrLn $ "Doubled number: " ++ show (num * 2)

    -- HC11T4
    let line1 = "Hello, "
    let line2 = "world!"
    putStrLn $ "Concatenated: " ++ line1 ++ line2

    -- HC11T5
    let inputs = ["hi", "test", "quit"]
    mapM_ (\inp -> if inp /= "quit" then putStrLn ("You said: " ++ inp) else return ()) inputs

    -- HC11T6
    let toConvert = "hello"
    putStrLn $ map toUpper toConvert

    -- HC11T7
    let option = "2"
    case option of
        "1" -> putStrLn "Hi there!"
        "2" -> let a = 10; b = 20 in putStrLn $ "Sum: " ++ show (a + b)
        "3" -> putStrLn "Goodbye!"
        _   -> putStrLn "Invalid option."

    -- HC11T8
    let checkNum = 4
    putStrLn $ if even checkNum then "Even" else "Odd"

    -- HC11T9
    let x = 3
    let y = 5
    putStrLn $ "Sum: " ++ show (x + y)

    -- HC11T10
    let inputStr = "haskell"
    putStrLn $ "Reversed: " ++ reverse inputStr
