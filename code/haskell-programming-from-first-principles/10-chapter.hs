import Debug.Trace

{- cabal:
build-depends: base, text, string-qq
-}
-- §stopVowelStop
stopVowelStop :: String -> String -> [(Char, Char, Char)]
stopVowelStop s1 s2 = [(a, b, c) | a <- s1, b <- s2, c <- s1]

stopVowelStopP :: String -> String -> [(Char, Char, Char)]
stopVowelStopP s1 s2 = filter (\(a, _, _) -> a == 'p') $ stopVowelStop s1 s2

-- §

-- §seekritFunc
seekritFunc :: (Fractional a) => String -> a
seekritFunc x = (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))

-- §

-- §myOr
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- §

-- §myAny
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

-- §

-- §myElem
myElem :: (Eq a) => a -> [a] -> Bool
myElem a = myAny (== a)

-- §

-- §myReverse
myReverse :: [a] -> [a]
myReverse = foldr (\x -> (++ [x])) []

-- §

-- §myMap
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- §

-- §myFilter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x xs -> if f x then (x : xs) else xs) []

-- §

-- §squish
squish :: [[a]] -> [a]
squish = foldr (++) []

-- §

-- §squishMap
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- §

-- §squishAgain
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- §

-- §myMaximumBy
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x : xs) = foldl (\a b -> if f a b == LT then b else a) x xs

-- §

-- §myMinimumBy
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x : xs) = foldl (\a b -> if f a b == GT then b else a) x xs

-- §
