import qualified Data.List as L
import qualified Data.Map as M
import Data.List (isInfixOf)
import qualified Data.Char as Char

-- Task 4 & 5: Function that sums only non-empty lists
sumNonEmpty :: [Int] -> Int
sumNonEmpty xs
    | null xs   = error "Cannot sum an empty list!"
    | otherwise = sum xs

main :: IO ()
main = do
    putStrLn "=== Task 1: Sorting Numbers ==="
    let numbers = [5, 1, 9, 3, 7]
    print (L.sort numbers)

    putStrLn "\n=== Task 2: Using Data.Map ==="
    let myMap = M.fromList [("apple", 3), ("banana", 5), ("cherry", 2)]
    print myMap

    putStrLn "\n=== Task 3: Sorting Map Keys ==="
    print (L.sort (M.keys myMap))

    putStrLn "\n=== Task 4 & 5: SumNonEmpty Function ==="
    let nums1 = [5, 15, 25]
    putStrLn $ "Sum of " ++ show nums1 ++ ": " ++ show (sumNonEmpty nums1)

    let nums2 = [0, 0, 0]
    putStrLn $ "Sum of " ++ show nums2 ++ ": " ++ show (sumNonEmpty nums2)

    -- Uncomment to trigger error for empty list
    -- let emptyList = []
    -- print (sumNonEmpty emptyList)

    putStrLn "\n=== Task 6: Directory Listing (Simulated) ==="
    let files = ["notes.txt", "report.txt", "image.png", "todo.txt", "data.csv"]
    print files

    putStrLn "\n=== Task 7 & 8: Filtering & Sorting Matching Files ==="
    let searchTerm = "txt"
    let lowerTerm  = map Char.toLower searchTerm
    let lowerFiles = map (map Char.toLower) files
    let matchingFiles =
            [ orig
            | (orig, lower) <- zip files lowerFiles
            , lowerTerm `isInfixOf` lower
            ]
    print (L.sort matchingFiles)

    putStrLn "\n=== Task 9: No Results Example ==="
    let noMatchTerm = "pdf"
    let lowerNoMatch = map Char.toLower noMatchTerm
    let noMatchFiles =
            [ orig
            | (orig, lower) <- zip files lowerFiles
            , lowerNoMatch `isInfixOf` lower
            ]
    if null noMatchFiles
        then putStrLn "No matching files found."
        else print (L.sort noMatchFiles)

    putStrLn "\n=== Task 10: Combined Usage ==="
    putStrLn $ "Sorted .txt files from the directory: " ++ show (L.sort matchingFiles)
