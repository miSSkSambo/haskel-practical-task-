-- HC10T1: ShowSimple Type Class
data PaymentMethod = Cash | Card | Crypto deriving Show

class ShowSimple a where
    showSimple :: a -> String

instance ShowSimple PaymentMethod where
    showSimple Cash   = "Cash"
    showSimple Card   = "Card"
    showSimple Crypto = "Crypto"

-- HC10T2: Summable Type Class
class Summable a where
    sumUp :: [a] -> a

instance Summable Int where
    sumUp = sum

-- HC10T3: Comparable Type Class
data Blockchain = Bitcoin | Ethereum | Solana deriving (Show)

class Comparable a where
    compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
    compareWith Bitcoin Bitcoin   = EQ
    compareWith Bitcoin _         = LT
    compareWith Ethereum Bitcoin  = GT
    compareWith Ethereum Ethereum = EQ
    compareWith Ethereum Solana   = LT
    compareWith Solana Solana     = EQ
    compareWith Solana _          = GT

-- HC10T4: Eq Instance for Box
data Box a = Empty | Has a deriving Show

instance Eq a => Eq (Box a) where
    Empty == Empty   = True
    Has x == Has y   = x == y
    _ == _           = False

-- HC10T5: ShowDetailed Type Class
data User = User { name :: String, email :: String }

class ShowDetailed a where
    showDetailed :: a -> String

instance ShowDetailed User where
    showDetailed (User n e) = "Name: " ++ n ++ ", Email: " ++ e

-- HC10T6: Mutual Recursion in Eq for Blockchain
instance Eq Blockchain where
    x == y = case (x, y) of
        (Bitcoin, Bitcoin)   -> True
        (Ethereum, Ethereum) -> True
        (Solana, Solana)     -> True
        _                    -> False
    x /= y = not (x == y)

-- HC10T7: Convertible Type Class
class Convertible a b where
    convert :: a -> b

instance Convertible PaymentMethod String where
    convert Cash   = "Paid with cash"
    convert Card   = "Paid with card"
    convert Crypto = "Paid with cryptocurrency"

-- HC10T8: AdvancedEq Subclass of Eq
class Eq a => AdvancedEq a where
    compareEquality :: a -> a -> Bool
    compareEquality x y = x == y

instance AdvancedEq Int

-- HC10T9: MinMax Type Class
class MinMax a where
    minValue :: a
    maxValue :: a

instance MinMax Int where
    minValue = minBound
    maxValue = maxBound

-- HC10T10: Concatenatable Type Class
class Concatenatable a where
    concatWith :: a -> a -> a

instance Concatenatable String where
    concatWith = (++)

-- MAIN
main :: IO ()
main = do
    -- Task 1: ShowSimple
    putStrLn "HC10T1: ShowSimple"
    putStrLn $ "Cash shown simply: " ++ showSimple Cash
    putStrLn ""

    -- Task 2: Summable
    putStrLn "HC10T2: Summable"
    print $ sumUp [10, 20, 30 :: Int]
    putStrLn ""

    -- Task 3: Comparable
    putStrLn "HC10T3: Comparable"
    print $ compareWith Bitcoin Ethereum
    putStrLn ""

    -- Task 4: Eq for Box
    putStrLn "HC10T4: Eq for Box"
    print $ Has 5 == Has 5
    print $ Has 5 == Has 6
    print $ (Empty :: Box Int) == Empty
    putStrLn ""

    -- Task 5: ShowDetailed
    putStrLn "HC10T5: ShowDetailed"
    let user1 = User "Katlego" "katlego@example.com"
    putStrLn $ showDetailed user1
    putStrLn ""

    -- Task 6: Mutual Eq
    putStrLn "HC10T6: Mutual Eq for Blockchain"
    print $ Ethereum == Solana
    print $ Ethereum /= Ethereum
    putStrLn ""

    -- Task 7: Convertible
    putStrLn "HC10T7: Convertible"
    putStrLn $ convert Card
    putStrLn ""

    -- Task 8: AdvancedEq
    putStrLn "HC10T8: AdvancedEq"
    print $ compareEquality (5 :: Int) 5
    print $ compareEquality (4 :: Int) 6
    putStrLn ""

    -- Task 9: MinMax
    putStrLn "HC10T9: MinMax"
    print (minValue :: Int)
    print (maxValue :: Int)
    putStrLn ""

    -- Task 10: Concatenatable
    putStrLn "HC10T10: Concatenatable"
    putStrLn $ concatWith "Hello, " "Haskell!"
