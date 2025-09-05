{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Strict
import Control.Monad.Reader
import Control.Monad.Identity
import qualified Data.Map.Strict as M
import Control.Exception (try, SomeException)
import Data.Char (isDigit, isUpper, isLower, isSpace)
import Data.Maybe (maybeToList)

--------------------------------------------------------------------------------
-- HC20T1: safeDivide with Maybe Monad
--------------------------------------------------------------------------------

safeDivide :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

--------------------------------------------------------------------------------
-- HC20T2: sequenceMaybe for List of Maybe
--------------------------------------------------------------------------------

sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe = foldr (liftA2 (:)) (Just [])

--------------------------------------------------------------------------------
-- HC20T3: Writer Monad Logging Calculator
--------------------------------------------------------------------------------

type Log = [String]
type Calc a = Writer Log a

logAdd :: Double -> Double -> Calc Double
logAdd x y = tell ["add " ++ show x ++ " + " ++ show y] >> pure (x + y)

logSub :: Double -> Double -> Calc Double
logSub x y = tell ["sub " ++ show x ++ " - " ++ show y] >> pure (x - y)

logMul :: Double -> Double -> Calc Double
logMul x y = tell ["mul " ++ show x ++ " * " ++ show y] >> pure (x * y)

logDiv :: Double -> Double -> Calc (Maybe Double)
logDiv x 0 = tell ["div " ++ show x ++ " / 0 -> error"] >> pure Nothing
logDiv x y = tell ["div " ++ show x ++ " / " ++ show y] >> pure (Just (x / y))

calcExample :: Double -> Double -> Double -> Double -> Calc (Maybe Double)
calcExample a b c d = do
  s  <- logAdd a b
  p  <- logMul s c
  logDiv p d

--------------------------------------------------------------------------------
-- HC20T4: countChars with State Monad
--------------------------------------------------------------------------------

countChars :: Char -> String -> Int
countChars target str =
  execState (forM_ str step) 0
  where
    step ch = when (ch == target) (modify (+1))

--------------------------------------------------------------------------------
-- HC20T5: Reader Monad for Configurable Greeting
--------------------------------------------------------------------------------

data GreetCfg = GreetCfg
  { greetingPrefix :: String
  , exclaim        :: Bool
  }

greet :: String -> Reader GreetCfg String
greet name = do
  pref <- asks greetingPrefix
  bang <- asks exclaim
  pure (pref ++ ", " ++ name ++ if bang then "!" else ".")

defaultCfg :: GreetCfg
defaultCfg = GreetCfg "Hello" True

--------------------------------------------------------------------------------
-- HC20T6: doubleMonad Combining Maybe and List
--------------------------------------------------------------------------------

doubleMonad :: Maybe a -> [b] -> [(a, b)]
doubleMonad mx ys = do
  x <- maybeToList mx
  y <- ys
  pure (x, y)

--------------------------------------------------------------------------------
-- HC20T7: findFirst with Either Monad
--------------------------------------------------------------------------------

findFirst :: (a -> Bool) -> [a] -> Either String a
findFirst p xs =
  case filter p xs of
    (y:_) -> Right y
    []    -> Left "No matching element found."

--------------------------------------------------------------------------------
-- HC20T8: Parser Monad for Simple Expressions
--------------------------------------------------------------------------------

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser g) = Parser $ \s -> do
    (a, rest) <- g s
    pure (f a, rest)

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  Parser pf <*> Parser pa = Parser $ \s -> do
    (f, s1) <- pf s
    (a, s2) <- pa s1
    pure (f a, s2)

instance Monad Parser where
  Parser pa >>= f = Parser $ \s -> do
    (a, s1) <- pa s
    runParser (f a) s1

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser $ \s -> p1 s <|> p2 s

item :: Parser Char
item = Parser $ \s -> case s of
  (c:cs) -> Just (c, cs)
  []     -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- item
  if p c then pure c else empty

char :: Char -> Parser Char
char c = satisfy (== c)

spaces :: Parser ()
spaces = () <$ many (satisfy isSpace)

token :: Parser a -> Parser a
token p = spaces *> p <* spaces

digit :: Parser Char
digit = satisfy isDigit

integer :: Parser Int
integer = token $ do
  sign <- optional (char '-')
  ds   <- some digit
  let n = read ds
  pure $ case sign of
           Just _  -> negate n
           Nothing -> n

parens :: Parser a -> Parser a
parens p = token (char '(') *> p <* token (char ')')

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = (do f <- op
                 y <- p
                 rest (f x y))
             <|> pure x

addop, mulop :: Parser (Int -> Int -> Int)
addop = token (char '+') *> pure (+)
mulop = token (char '*') *> pure (*)

factor :: Parser Int
factor = integer <|> parens expr

term :: Parser Int
term = factor `chainl1` mulop

expr :: Parser Int
expr = term `chainl1` addop

parseExpr :: String -> Maybe Int
parseExpr s = do
  (v, rest) <- runParser (spaces *> expr <* spaces) s
  if null rest then Just v else Nothing

--------------------------------------------------------------------------------
-- HC20T9: replicateMonad with Identity Monad
--------------------------------------------------------------------------------

replicateMonad :: Int -> a -> Identity [a]
replicateMonad n x = replicateM n (Identity x)

--------------------------------------------------------------------------------
-- HC20T10: Nested StateT and MaybeT Transformer
--------------------------------------------------------------------------------

type StackM a = StateT [a] (MaybeT Identity)

mzeroT :: MaybeT Identity b
mzeroT = MaybeT (Identity Nothing)

popSafe :: StackM a a
popSafe = do
  st <- get
  case st of
    []     -> lift mzeroT
    (x:xs) -> put xs >> pure x

popN :: Int -> StackM a [a]
popN n
  | n <= 0    = pure []
  | otherwise = do
      x  <- popSafe
      xs <- popN (n - 1)
      pure (x:xs)

runStackM :: StackM a b -> [a] -> Maybe (b, [a])
runStackM m s =
  case runIdentity (runMaybeT (runStateT m s)) of
    Nothing       -> Nothing
    Just (b, s')  -> Just (b, s')

--------------------------------------------------------------------------------
-- HC20T11: randomWalk (deterministic, no System.Random)
--------------------------------------------------------------------------------

type Point = (Int, Int)

stepSeq :: [Point]
stepSeq = cycle [(1,0), (-1,0), (0,1), (0,-1)]

randomWalk :: Int -> [Point]
randomWalk n = take (n+1) (scanl step (0,0) (take n stepSeq))
  where
    step (x,y) (dx,dy) = (x+dx, y+dy)

--------------------------------------------------------------------------------
-- HC20T12: File Reading with IO Monad
--------------------------------------------------------------------------------

readFileLines :: FilePath -> IO ()
readFileLines fp = do
  contents <- readFile fp
  let ls = lines contents
  forM_ (zip [1 :: Int ..] ls) $ \(i, line) ->
    putStrLn (show i ++ ": " ++ line)

--------------------------------------------------------------------------------
-- HC20T13: fibonacciMemo with State Monad
--------------------------------------------------------------------------------

fibonacciMemo :: Int -> State (M.Map Int Integer) Integer
fibonacciMemo n
  | n <= 1 = pure (fromIntegral n)
  | otherwise = do
      memo <- get
      case M.lookup n memo of
        Just v  -> pure v
        Nothing -> do
          a <- fibonacciMemo (n-1)
          b <- fibonacciMemo (n-2)
          let v = a + b
          modify (M.insert n v)
          pure v

runFibonacciMemo :: Int -> Integer
runFibonacciMemo n = evalState (fibonacciMemo n) M.empty

--------------------------------------------------------------------------------
-- HC20T14: mapMFilter Monadic Map-Filter
--------------------------------------------------------------------------------

mapMFilter :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMFilter f = foldr step (pure [])
  where
    step a acc = do
      mb <- f a
      bs <- acc
      case mb of
        Nothing -> pure bs
        Just b  -> pure (b:bs)

--------------------------------------------------------------------------------
-- HC20T15: treeSum with Custom Monad
--------------------------------------------------------------------------------

data Tree
  = Leaf Int
  | Node Tree Tree
  deriving (Show, Eq)

newtype SumM a = SumM { runSumM :: (Int, a) }

instance Functor SumM where
  fmap f (SumM (s, a)) = SumM (s, f a)

instance Applicative SumM where
  pure a = SumM (0, a)
  SumM (s1, f) <*> SumM (s2, a) = SumM (s1 + s2, f a)

instance Monad SumM where
  SumM (s1, a) >>= k =
    let SumM (s2, b) = k a
    in SumM (s1 + s2, b)

tellSum :: Int -> SumM ()
tellSum x = SumM (x, ())

treeSum :: Tree -> SumM Int
treeSum (Leaf n) = tellSum n >> pure n
treeSum (Node l r) = do
  sl <- treeSum l
  sr <- treeSum r
  pure (sl + sr)

runTreeSum :: Tree -> (Int, Int)
runTreeSum t = runSumM (treeSum t)

--------------------------------------------------------------------------------
-- HC20T16: retryIO with IO Monad
--------------------------------------------------------------------------------

retryIO :: forall a. Int -> IO a -> IO (Maybe a)
retryIO n action
  | n <= 0    = pure Nothing
  | otherwise = do
      r <- (try action :: IO (Either SomeException a))
      case r of
        Right v -> pure (Just v)
        Left _  -> retryIO (n - 1) action

--------------------------------------------------------------------------------
-- HC20T17: validatePassword with Either Monad
--------------------------------------------------------------------------------

validatePassword :: String -> Either [String] String
validatePassword pwd =
  let checks =
        [ (length pwd >= 8,  "Password must be at least 8 characters.")
        , (any isDigit pwd,  "Password must contain at least one digit.")
        , (any isUpper pwd,  "Password must contain at least one uppercase letter.")
        , (any isLower pwd,  "Password must contain at least one lowercase letter.")
        ]
      errs = [ msg | (ok, msg) <- checks, not ok ]
  in if null errs then Right pwd else Left errs

--------------------------------------------------------------------------------
-- HC20T18: MaybeT Monad Transformer for User Input
--------------------------------------------------------------------------------

getNonEmptyLine :: String -> MaybeT IO String
getNonEmptyLine prompt = MaybeT $ do
  putStr prompt
  line <- getLine
  pure $ if all isSpace line then Nothing else Just line

getIntMaybe :: String -> MaybeT IO Int
getIntMaybe prompt = MaybeT $ do
  putStr prompt
  s <- getLine
  pure $ case reads s of
    [(n,"")] -> Just n
    _        -> Nothing

--------------------------------------------------------------------------------
-- HC20T19: Writer Monad-based Logging System
--------------------------------------------------------------------------------

logFn1 :: Show a => String -> (a -> b) -> a -> Writer Log b
logFn1 name f a = tell [name ++ "(" ++ show a ++ ")"] >> pure (f a)

logFn2 :: (Show a, Show b) => String -> (a -> b -> c) -> a -> b -> Writer Log c
logFn2 name f a b = tell [name ++ "(" ++ show a ++ "," ++ show b ++ ")"] >> pure (f a b)

--------------------------------------------------------------------------------
-- HC20T20: batchProcessing with Monadic Bind (>>=)
--------------------------------------------------------------------------------

batchProcessing :: Monad m => (a -> m b) -> [a] -> m [b]
batchProcessing f =
  foldr (\x acc -> f x >>= \y -> acc >>= \ys -> pure (y:ys)) (pure [])

--------------------------------------------------------------------------------
-- Demo in main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "HC20T1 safeDivide:"
  print (safeDivide 10 (2 :: Double), safeDivide 5 0)

  putStrLn "\nHC20T2 sequenceMaybe:"
  print (sequenceMaybe [Just 1, Just 2, Just 3] :: Maybe [Int])
  print (sequenceMaybe [Just 1, Nothing, Just 3] :: Maybe [Int])

  putStrLn "\nHC20T3 Writer logging calculator:"
  let (res, logMsgs) = runWriter (calcExample 3 4 10 2)
  print res
  mapM_ putStrLn logMsgs

  putStrLn "\nHC20T4 countChars:"
  print (countChars 'a' "bananas")

  putStrLn "\nHC20T5 Reader greeting:"
  print (runReader (greet "KJ") defaultCfg)

  putStrLn "\nHC20T6 doubleMonad:"
  print (doubleMonad (Just 'X') [1..4 :: Int])
  print (doubleMonad (Nothing :: Maybe Char) [1..4 :: Int])  -- FIXED: added type

  putStrLn "\nHC20T7 findFirst:"
  print (findFirst even [1,3,4,6 :: Int])
  print (findFirst (>10) [1,3,4,6 :: Int])

  putStrLn "\nHC20T8 Parser (expr):"
  print (parseExpr "2 + 3 * 4")
  print (parseExpr "(2+3)*4")
  print (parseExpr " 10 + (6 * 5) + 1 ")

  putStrLn "\nHC20T9 replicateMonad:"
  print (runIdentity (replicateMonad 5 'a'))

  putStrLn "\nHC20T10 StateT + MaybeT (stack pop):"
  print (runStackM (popN 3) [10,20,30,40 :: Int])
  print (runStackM (popN 5) [10,20,30,40 :: Int])

  putStrLn "\nHC20T11 randomWalk (first 10 points):"
  print (randomWalk 10)

  putStrLn "\nHC20T12 File reading (demo skipped; call readFileLines \"path.txt\")"

  putStrLn "\nHC20T13 fibonacciMemo:"
  print (runFibonacciMemo 20)

  putStrLn "\nHC20T14 mapMFilter (keep even squares):"
  let f n = pure (if even n then Just (n*n) else Nothing) :: IO (Maybe Int)
  print =<< mapMFilter f [1..10 :: Int]

  putStrLn "\nHC20T15 treeSum with custom monad:"
  let t = Node (Leaf 5) (Node (Leaf 7) (Leaf 8))
  print (runTreeSum t)

  putStrLn "\nHC20T16 retryIO (example with safe action):"
  r <- retryIO 3 (pure (99 :: Int))
  print r

  putStrLn "\nHC20T17 validatePassword:"
  print (validatePassword "Abcdefg1")
  print (validatePassword "short")

  putStrLn "\nHC20T19 Writer-based logging system:"
  let (z, lg) = runWriter $ do
        a <- logFn1 "succ" succ (10 :: Int)
        logFn2 "add" (+) a 5
  print z
  mapM_ putStrLn lg

  putStrLn "\nHC20T20 batchProcessing:"
  xs <- batchProcessing (\n -> putStrLn ("processing " ++ show n) >> pure (n*n)) [1..5 :: Int]
  print xs
