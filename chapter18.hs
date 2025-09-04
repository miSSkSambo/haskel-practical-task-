import Data.Char (toLower)

-- HC18T1: mapToLower Function with fmap
mapToLower :: String -> String
mapToLower = fmap toLower


-- HC18T2: Functor Instance for Tree
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)


-- HC18T3: incrementTreeValues Function
incrementTreeValues :: Num a => Tree a -> Tree a
incrementTreeValues = fmap (+1)


-- HC18T4: mapToBits Function
mapToBits :: [Bool] -> [Char]
mapToBits = fmap (\b -> if b then '1' else '0')


-- HC18T5: Functor Instance for Either
-- Already defined in Prelude, so we just use it.


-- HC18T6: applyToMaybe Function
applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe = fmap


-- HC18T7: fmapTuple Function
fmapTuple :: (b -> c) -> (a, b) -> (a, c)
fmapTuple = fmap


-- HC18T8: identityLawCheck Function
identityLawCheck :: (Functor f, Eq (f a)) => f a -> Bool
identityLawCheck x = fmap id x == x


-- HC18T9: compositionLawCheck Function
compositionLawCheck :: (Functor f, Eq (f c)) 
                    => (b -> c) -> (a -> b) -> f a -> Bool
compositionLawCheck f g x = fmap (f . g) x == (fmap f . fmap g) x


-- HC18T10: nestedFmap Function
nestedFmap :: (a -> b) -> [[Maybe a]] -> [[Maybe b]]
nestedFmap = fmap . fmap . fmap


-- Example Tree for testing
exampleTree :: Tree Int
exampleTree = Node 5 (Node 3 Empty (Node 4 Empty Empty)) (Node 7 Empty Empty)


-- Main to test all tasks with labeled output
main :: IO ()
main = do
    -- HC18T1
    putStrLn "HC18T1 :"
    print $ mapToLower "HELLO Functors!"  

    -- HC18T2
    putStrLn "\nHC18T2 :"
    print exampleTree

    -- HC18T3
    putStrLn "\nHC18T3 :"
    print $ incrementTreeValues exampleTree

    -- HC18T4
    putStrLn "\nHC18T4 :"
    print $ mapToBits [True, False, True, True]

    -- HC18T5
    putStrLn "\nHC18T5 :"
    print $ fmap (+1) (Right 5 :: Either String Int)
    print $ fmap (+1) (Left "error" :: Either String Int)

    -- HC18T6
    putStrLn "\nHC18T6 :"
    print $ applyToMaybe (*2) (Just 10)
    print $ applyToMaybe (*2) Nothing

    -- HC18T7
    putStrLn "\nHC18T7 :"
    print $ fmapTuple show (1, 100)

    -- HC18T8
    putStrLn "\nHC18T8 :"
    print $ identityLawCheck (Just 5)
    print $ identityLawCheck [1,2,3]

    -- HC18T9
    putStrLn "\nHC18T9 :"
    print $ compositionLawCheck (+1) (*2) (Just 3)
    print $ compositionLawCheck (+1) (*2) [1,2,3]

    -- HC18T10
    putStrLn "\nHC18T10 :"
    print $ nestedFmap (+1) [[Just 1, Nothing], [Just 3]]
