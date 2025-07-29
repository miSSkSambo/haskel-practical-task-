-- Chapter 9: Parametric Types and Recursion

-- HC9T1: Define a Parametric Type Synonym
type Entity a = (String, a)

-- HC9T2: Parametric Data Type Box
data Box a = Empty | Has a deriving (Show)

-- HC9T3: Add value to Box if it has a number
addN :: Num a => a -> Box a -> Box a
addN n (Has x) = Has (x + n)
addN _ Empty   = Empty

-- HC9T4: Extract value or use default
extract :: a -> Box a -> a
extract def (Has x) = x
extract def Empty   = def

-- HC9T5: Parametric Shape type with color
data Shape a = Circle { radius :: Float, color :: a }
             | Rectangle { width :: Float, height :: Float, color :: a }
             deriving Show

-- HC9T6: Recursive Tweet data type
data Tweet = Tweet
  { content  :: String
  , likes    :: Int
  , comments :: [Tweet]
  } deriving Show

-- HC9T7: Engagement function (likes + nested comment likes)
engagement :: Tweet -> Int
engagement (Tweet _ l cs) = l + sum (map engagement cs)

-- HC9T8: Recursive Sequence type
data Sequence a = End | Node a (Sequence a) deriving Show

-- HC9T9: Check if element is in Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq x (Node y ys) = x == y || elemSeq x ys

-- HC9T10: Binary Search Tree type
data BST a = Leaf
           | Branch a (BST a) (BST a)
           deriving Show

-- Insert into BST
insertBST :: Ord a => a -> BST a -> BST a
insertBST x Leaf = Branch x Leaf Leaf
insertBST x (Branch v l r)
    | x < v     = Branch v (insertBST x l) r
    | x > v     = Branch v l (insertBST x r)
    | otherwise = Branch v l r

-- Search in BST
searchBST :: Ord a => a -> BST a -> Bool
searchBST _ Leaf = False
searchBST x (Branch v l r)
    | x == v    = True
    | x < v     = searchBST x l
    | otherwise = searchBST x r

-- === MAIN ===
main :: IO ()
main = do
    putStrLn "== HC9T1: Entity type =="
    let wallet :: Entity Int
        wallet = ("Alice", 100)
    print wallet

    putStrLn "\n== HC9T2 & HC9T3: Box addN =="
    let box = Has 10
    print (addN 5 box)

    putStrLn "\n== HC9T4: extract =="
    print (extract 0 Empty)
    print (extract 0 (Has 42))

    putStrLn "\n== HC9T5: Shape with color =="
    let redCircle = Circle 5.0 "Red"
    let blueRect = Rectangle 4.0 6.0 "Blue"
    print redCircle
    print blueRect

    putStrLn "\n== HC9T6 & HC9T7: Tweet engagement =="
    let t1 = Tweet "Hello" 10 []
    let t2 = Tweet "Reply" 5 []
    let t3 = Tweet "Thread" 2 [t1, t2]
    print (engagement t3)

    putStrLn "\n== HC9T8 & HC9T9: Sequence =="
    let seq1 = Node 1 (Node 2 (Node 3 End))
    print (elemSeq 2 seq1)
    print (elemSeq 4 seq1)

    putStrLn "\n== HC9T10: BST insert and search =="
    let tree = foldr insertBST Leaf [5, 3, 8, 1, 4]
    print tree
    print (searchBST 4 tree)
    print (searchBST 9 tree)
