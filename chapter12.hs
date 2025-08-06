import Data.Char (toLower, isAlphaNum)
import Data.List (sort)

main :: IO ()
main = do
    -- HC12T1: Welcome message
    print "Welcome to Haskell Programming!"

    -- HC12T2: Add two numbers
    let a = 7
    let b = 3
    print ("Sum of two numbers: " ++ show (a + b))

    -- HC12T3: Factorial of 5
    let factorial n = if n == 0 then 1 else n * factorial (n - 1)
    print ("Factorial of 5: " ++ show (factorial 5))

    -- HC12T4: First 10 Fibonacci numbers
    let fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)
    let fibList = map fib [0..9]
    print ("First 10 Fibonacci numbers: " ++ show fibList)

    -- HC12T5: Palindrome check
    let input = "A man, a plan, a canal: Panama"
    let cleaned = map toLower (filter isAlphaNum input)
    let isPal = cleaned == reverse cleaned
    print ("Is palindrome: " ++ show isPal)

    -- HC12T6: Sort list
    let unsortedList = [4, 1, 7, 3, 2]
    print ("Sorted list: " ++ show (sort unsortedList))

    -- HC12T7: Calculate circle area
    let radius = 5.0
    let area = pi * radius * radius
    print ("Area of circle with radius 5: " ++ show area)

    -- HC12T8: Merge two sorted lists
    let list1 = [1, 3, 5]
    let list2 = [2, 4, 6]
    let merged = sort (list1 ++ list2)
    print ("Merged sorted list: " ++ show merged)

    -- HC12T9: Read and print file content (simplified, no file I/O due to editor limitation)
    print "Skipping file read (HC12T9) - File I/O needs real environment."

    -- HC12T10: Simple math operations
    let x = 10
    let y = 5
    print ("Addition: " ++ show (x + y))
    print ("Subtraction: " ++ show (x - y))
    print ("Multiplication: " ++ show (x * y))
    print ("Division: " ++ show (x `div` y))
