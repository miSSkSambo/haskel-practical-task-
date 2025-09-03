{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- Chapter 17: Semigroups and Monoids — Tasks 1..10

import Data.Semigroup (Semigroup(..))
import Data.Monoid    (Monoid(..))
import qualified Data.List as L
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- HC17T1 + HC17T3: Severity type with Semigroup & Monoid
-- T1 rule: higher severity wins (Semigroup: max).
-- T3 identity: Low (Monoid).

data Severity = Low | Medium | High | Critical
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance Semigroup Severity where
  (<>) = max                      -- higher overrides lower

instance Monoid Severity where     -- (HC17T3)
  mempty  = Low                    -- identity
  mappend = (<>)

--------------------------------------------------------------------------------
-- HC17T2: Min and Max newtypes with Semigroup via min/max

newtype Min' a = Min' { getMin' :: a } deriving (Eq, Ord, Show)
newtype Max' a = Max' { getMax' :: a } deriving (Eq, Ord, Show)

instance Ord a => Semigroup (Min' a) where
  Min' x <> Min' y = Min' (min x y)

instance Ord a => Semigroup (Max' a) where
  Max' x <> Max' y = Max' (max x y)

--------------------------------------------------------------------------------
-- HC17T4: Monoid for Sum newtype (identity = 0)
-- (Avoid clash with Data.Monoid.Sum: name it Sum')

newtype Sum' a = Sum' { getSum' :: a } deriving (Eq, Ord, Show)

instance Num a => Semigroup (Sum' a) where
  Sum' x <> Sum' y = Sum' (x + y)

instance Num a => Monoid (Sum' a) where
  mempty  = Sum' 0
  mappend = (<>)

--------------------------------------------------------------------------------
-- HC17T7: Product newtype + Monoid (identity = 1)
-- (Avoid clash with Data.Monoid.Product: name it Product')

newtype Product' a = Product' { getProduct' :: a } deriving (Eq, Ord, Show)

instance Num a => Semigroup (Product' a) where
  Product' x <> Product' y = Product' (x * y)

instance Num a => Monoid (Product' a) where
  mempty  = Product' 1
  mappend = (<>)

-- HC17T7 function
multiplyProducts :: Num a => [Product' a] -> Product' a
multiplyProducts = mconcat

--------------------------------------------------------------------------------
-- HC17T5: combineLists using list Semigroup (<> == ++)

combineLists :: [Int] -> [Int] -> [Int]
combineLists = (<>)

--------------------------------------------------------------------------------
-- HC17T6: maxSeverity using mconcat (uses Monoid Severity)

maxSeverity :: [Severity] -> Severity
maxSeverity = mconcat

--------------------------------------------------------------------------------
-- HC17T8: foldWithSemigroup via foldr1 (non-empty list required)

foldWithSemigroup :: Semigroup a => [a] -> a
foldWithSemigroup = foldr1 (<>)

--------------------------------------------------------------------------------
-- HC17T9 + HC17T10: Config with Semigroup & Monoid
-- Semigroup combine (HC17T9):
--   loggingLevel = max, timeout = min, retries = max
-- Monoid identity (HC17T10):
--   loggingLevel = 0 (lowest), timeout = maxBound (highest), retries = 0 (lowest)

data Config = Config
  { loggingLevel :: Int   -- 0 = least verbose
  , timeout      :: Int   -- milliseconds
  , retries      :: Int   -- retry attempts
  } deriving (Eq, Show)

instance Semigroup Config where      -- (HC17T9)
  a <> b = Config
    { loggingLevel = max (loggingLevel a) (loggingLevel b)
    , timeout      = min (timeout      a) (timeout      b)
    , retries      = max (retries      a) (retries      b)
    }

instance Monoid Config where         -- (HC17T10)
  mempty = Config
    { loggingLevel = 0
    , timeout      = maxBound
    , retries      = 0
    }
  mappend = (<>)

--------------------------------------------------------------------------------
-- Demos

main :: IO ()
main = do
  putStrLn "=== HC17T1/T3: Severity Semigroup & Monoid ==="
  print (High <> Medium)                             -- High
  print (mconcat [Low, High, Medium, Critical, Low]) -- Critical
  print (mempty <> Medium)                           -- Medium

  putStrLn "\n=== HC17T2: Min'/Max' Semigroups ==="
  print (Min' (10 :: Int) <> Min' 3)  -- Min' 3
  print (Max' (10 :: Int) <> Max' 3)  -- Max' 10

  putStrLn "\n=== HC17T4: Sum' Monoid ==="
  print (mconcat [Sum' 5, Sum' 7, Sum' (-2)])  -- Sum' 10
  print (mempty :: Sum' Int)                    -- Sum' 0

  putStrLn "\n=== HC17T5: combineLists ==="
  print (combineLists [1,2,3] [4,5])           -- [1,2,3,4,5]

  putStrLn "\n=== HC17T6: maxSeverity ==="
  print (maxSeverity [Low, Medium, High, Medium]) -- High
  print (maxSeverity [])                           -- Low (identity)

  putStrLn "\n=== HC17T7: multiplyProducts ==="
  print (multiplyProducts [Product' 2, Product' 3, Product' 4]) -- Product' 24
  print (mempty :: Product' Int)                                -- Product' 1

  putStrLn "\n=== HC17T8: foldWithSemigroup ==="
  print (foldWithSemigroup ["Hi", " ", "there", "!"]) -- "Hi there!"
  print (foldWithSemigroup [Max' 1, Max' 20, Max' 3]) -- Max' 20
  -- foldWithSemigroup []  -- would crash (like foldr1)

  putStrLn "\n=== HC17T9/T10: Config Semigroup & Monoid ==="
  let c1 = Config { loggingLevel = 1, timeout = 5000, retries = 1 }
      c2 = Config { loggingLevel = 3, timeout = 2000, retries = 0 }
      c3 = Config { loggingLevel = 2, timeout = 8000, retries = 4 }
  print (c1 <> c2)                   -- combine (max, min, max)
  print (mconcat [c1, c2, c3])       -- combine three
  print (mempty <> c1)               -- identity
  print (c1 <> mempty)               -- identity
