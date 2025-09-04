-- Applicatives & Effects — Tasks HC11..HC19 (+HC18)
-- Single-file, base-only solution

import Control.Applicative (liftA2, liftA3)
import Control.Monad       (forever, when, replicateM)

--------------------------------------------------------------------------------
-- HC19T1: Applicative Instance for Pair
-- Elementwise applicative (like a fixed-size zip of length 2)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  Pair f g <*> Pair x y = Pair (f x) (g y)

--------------------------------------------------------------------------------
-- HC19T2: addThreeApplicative (Maybe Int)
addThreeApplicative :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addThreeApplicative = liftA3 (\a b c -> a + b + c)
-- or: pure (\a b c -> a + b + c) <*> m1 <*> m2 <*> m3

--------------------------------------------------------------------------------
-- HC19T3: safeProduct for [Maybe Int]
safeProduct :: [Maybe Int] -> Maybe Int
safeProduct = fmap product . sequenceA
-- sequenceA :: [Maybe Int] -> Maybe [Int]

--------------------------------------------------------------------------------
-- HC19T4: liftAndMultiply with liftA2
liftAndMultiply :: Applicative f => (Int -> Int -> Int) -> f Int -> f Int -> f Int
liftAndMultiply = liftA2

--------------------------------------------------------------------------------
-- HC19T5: applyEffects with <*>
-- Takes (IO Int, IO Int), prints each value as it’s produced, returns their sum.
applyEffects :: (IO Int, IO Int) -> IO Int
applyEffects (aIO, bIO) =
  let logValue io = do n <- io; print n; pure n
  in  (+) <$> logValue aIO <*> logValue bIO

--------------------------------------------------------------------------------
-- HC19T6: repeatEffect with forever  (infinite loop; don’t call in main)
repeatEffect :: IO a -> IO b
repeatEffect = forever

--------------------------------------------------------------------------------
-- HC19T7: conditionalPrint with when
conditionalPrint :: Bool -> String -> IO ()
conditionalPrint cond msg = when cond (putStrLn msg)

--------------------------------------------------------------------------------
-- HC19T8: discardSecond with <*
discardSecond :: Applicative f => f a -> f b -> f a
discardSecond = (<*)

--------------------------------------------------------------------------------
-- HC19T9: pureAndApply Demonstration (Maybe)
pureAndApply :: Maybe Int -> Maybe Int -> Maybe Int
pureAndApply x y = pure (+) <*> x <*> y
-- Same as: liftA2 (+) x y

--------------------------------------------------------------------------------
-- HC19T10: combineResults for Either (Right applies, Left propagates)
combineResults :: Either e Int -> Either e Int -> Either e Int
combineResults a b = pure (+) <*> a <*> b

--------------------------------------------------------------------------------
-- HC11T1: Applicative Instance for Wrapper
data Wrapper a = Wrapper a deriving (Eq, Show)

instance Functor Wrapper where
  fmap f (Wrapper x) = Wrapper (f x)

instance Applicative Wrapper where
  pure = Wrapper
  Wrapper f <*> Wrapper x = Wrapper (f x)

--------------------------------------------------------------------------------
-- HC11T2: sumThreeApplicative for Either String Int
sumThreeApplicative
  :: Either String Int -> Either String Int -> Either String Int
  -> Either String Int
sumThreeApplicative = liftA3 (\a b c -> a + b + c)

--------------------------------------------------------------------------------
-- HC12T1: whenApplicative Function
whenApplicative :: Bool -> IO () -> IO ()
whenApplicative = when

--------------------------------------------------------------------------------
-- HC12T2: replicateEffect with replicateM
replicateEffect :: Int -> IO a -> IO [a]
replicateEffect = replicateM

--------------------------------------------------------------------------------
-- HC13T1: sequenceEffects for Applicative List
sequenceEffects :: Applicative f => [f a] -> f [a]
sequenceEffects = sequenceA

--------------------------------------------------------------------------------
-- HC14T1: applyWithEffects and <*>
applyWithEffects :: Applicative f => f (a -> b) -> f a -> f b
applyWithEffects = (<*>)

--------------------------------------------------------------------------------
-- HC15T1: simulateMaybeEffect for multiple Maybe
simulateMaybeEffect
  :: (a -> b -> c -> d)
  -> Maybe a -> Maybe b -> Maybe c -> Maybe d
simulateMaybeEffect f ma mb mc = pure f <*> ma <*> mb <*> mc
-- or: liftA3 f ma mb mc

--------------------------------------------------------------------------------
-- HC16T1: combineEitherResults with Multiple Either
combineEitherResults :: Either e a -> Either e b -> Either e (a, b)
combineEitherResults = liftA2 (,)

--------------------------------------------------------------------------------
-- HC17T1: sequenceApplicative for Maybe List
sequenceApplicative :: [Maybe a] -> Maybe [a]
sequenceApplicative = sequenceA

--------------------------------------------------------------------------------
-- HC18T1: replicateForever with forever (infinite loop; don’t call in main)
replicateForever :: IO a -> IO b
replicateForever = forever

--------------------------------------------------------------------------------
-- Demos

main :: IO ()
main = do
  putStrLn "=== HC19T1: Pair Applicative ==="
  print (pure 10 :: Pair Int)
  print (Pair (+1) (*2) <*> Pair 5 7)  -- Pair 6 14

  putStrLn "\n=== HC19T2: addThreeApplicative (Maybe) ==="
  print (addThreeApplicative (Just 1) (Just 2) (Just 3))  -- Just 6
  print (addThreeApplicative (Just 1) Nothing (Just 3))   -- Nothing

  putStrLn "\n=== HC19T3: safeProduct [Maybe Int] ==="
  print (safeProduct [Just 2, Just 3, Just 4])            -- Just 24
  print (safeProduct [Just 2, Nothing, Just 4])           -- Nothing

  putStrLn "\n=== HC19T4: liftAndMultiply with liftA2 ==="
  print (liftAndMultiply (*) (Just 6) (Just 7))           -- Just 42

  putStrLn "\n=== HC19T5: applyEffects (prints, then sums) ==="
  sumAB <- applyEffects (pure 10, pure 5)                 -- prints 10 then 5
  print sumAB                                             -- 15

  putStrLn "\n=== HC19T7: conditionalPrint (when) ==="
  conditionalPrint True  "Condition is True!"
  conditionalPrint False "You should not see this."

  putStrLn "\n=== HC19T8: discardSecond (<*) ==="
  _ <- discardSecond (putStrLn "first") (putStrLn "second")
  -- prints "first" then "second", returns result of first action
  pure ()

  putStrLn "\n=== HC19T9: pureAndApply (Maybe) ==="
  print (pureAndApply (Just 3) (Just 4))                  -- Just 7
  print (pureAndApply (Just 3) (Nothing :: Maybe Int))    -- Nothing

  putStrLn "\n=== HC19T10: combineResults (Either) ==="
  print (combineResults (Right 2) (Right 3) :: Either String Int) -- Right 5
  print (combineResults (Left "err") (Right 3) :: Either String Int)

  putStrLn "\n=== HC11T1: Wrapper Applicative ==="
  print (Wrapper (+1) <*> Wrapper 41)                     -- Wrapper 42
  print (fmap (*3) (Wrapper 7))                           -- Wrapper 21

  putStrLn "\n=== HC11T2: sumThreeApplicative (Either String Int) ==="
  print (sumThreeApplicative (Right 1) (Right 2) (Right 3))        -- Right 6
  print (sumThreeApplicative (Left "oops") (Right 2) (Right 3))    -- Left "oops"

  putStrLn "\n=== HC12T1: whenApplicative ==="
  whenApplicative True  (putStrLn "Ran because True")
  whenApplicative False (putStrLn "Won't run")

  putStrLn "\n=== HC12T2: replicateEffect (replicateM) ==="
  xs <- replicateEffect 3 (pure 7)
  print xs                                                -- [7,7,7]

  putStrLn "\n=== HC13T1: sequenceEffects ==="
  print (sequenceEffects [Just 1, Just 2, Just 3])        -- Just [1,2,3]
  print (sequenceEffects [Right 'a', Right 'b'] :: Either String [Char])

  putStrLn "\n=== HC14T1: applyWithEffects (<*>) ==="
  print (applyWithEffects (Just negate) (Just 10))        -- Just (-10)

  putStrLn "\n=== HC15T1: simulateMaybeEffect (3 args) ==="
  print (simulateMaybeEffect (\a b c -> a + b * c)
                             (Just 2) (Just 3) (Just 4))  -- Just 14
  print (simulateMaybeEffect (\a b c -> a + b * c)
                             (Just 2) (Nothing :: Maybe Int) (Just 4)) -- Nothing

  putStrLn "\n=== HC16T1: combineEitherResults (2 values) ==="
  print (combineEitherResults (Right 'x') (Right 99) :: Either String (Char, Int))
  print (combineEitherResults (Left "bad") (Right 99)  :: Either String (Char, Int))

  putStrLn "\n=== HC17T1: sequenceApplicative (Maybe list) ==="
  print (sequenceApplicative [Just 1, Just 2, Just 3])    -- Just [1,2,3]
  print (sequenceApplicative [Just 1, Nothing, Just 3])   -- Nothing

  putStrLn "\n=== HC18T1: replicateForever (forever) ==="
  putStrLn "(Not running replicateForever/repeatEffect here to avoid an infinite loop.)"
