import Data.List (intersperse)

{- cabal:
build-depends: base, text, string-qq
-}
-- §dividedBy
data DividedResult = Result Integer | DividedByZero deriving (Show)

dividedBy :: (Integral a) => a -> a -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy num denom = Result (go num denom 0)
  where
    go n d count
      | abs n < abs d = count
      | abs (n + d) == abs (n) + abs (d) = go (n - d) d (count + 1)
      | otherwise = go (n + d) d (count - 1)

-- §
-- §mc91
mc91 :: Int -> Int
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 (mc91 (n + 11))

-- §

-- §numbersIntoWords

digitToWord :: Int -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"

digits :: Int -> [Int]
digits 0 = []
digits n = n `mod` 10 : digits (n `div` 10)

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ reverse $ digits n

hellow this is a realllllllllly long line of text getting longer and longer and reallllllly lomng now

-- §
