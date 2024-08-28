import Data.Char
import Debug.Trace

{- cabal:
build-depends: base, text, string-qq
-}
-- §filterUpper
filterUpper :: String -> String
filterUpper = filter isUpper

-- §

-- §capitalise
capitalise :: String -> String
capitalise (x : xs) = toUpper x : xs

-- §

-- §shout
shout :: String -> String
shout [] = []
shout (x : xs) = toUpper x : shout xs

-- §

-- §capitaliseHead
capitaliseHead :: String -> Char
capitaliseHead = toUpper . head

-- §

-- §cypher
caeserChar :: Int -> Char -> Char
caeserChar rot c = chr $ 97 + mod (ord c + rot - 97) 26

caeser :: Int -> String -> String
caeser rot s = map (caeserChar rot) s

unCaeser :: Int -> String -> String
unCaeser rot = caeser (-rot)

-- §

-- §myOr
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = x || myOr xs

-- §

-- §myAny
myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr $ map f xs

-- §

-- §myElem
myElem :: (Eq a) => a -> [a] -> Bool
myElem a xs = myAny (== a) xs

-- §

-- §myReverse
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- §

-- §squish
squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

-- §

-- §squishMap
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x : xs) = f x ++ squishMap f xs

-- §

-- §squishMap
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- §
