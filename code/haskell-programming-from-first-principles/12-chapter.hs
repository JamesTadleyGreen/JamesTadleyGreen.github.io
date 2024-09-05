import Data.Char
import Data.List (elemIndex, intersperse)
import Debug.Trace

{- cabal:
build-depends: base, text, string-qq
-}

-- §replaceThe
notThe :: String -> Maybe String
notThe s = if s == "the" then Nothing else Just s

replaceThe :: String -> String
replaceThe s = go $ words s
  where
    go [] = []
    go (s : ss)
      | notThe s == Nothing = "a" ++ " " ++ go ss
      | otherwise = s ++ " " ++ go ss

-- §

-- §countTheBeforeVowel
vowels = "aeiou"

countTheBeforeVowel :: String -> Int
countTheBeforeVowel s = go (words s) 0
  where
    go [] i = i
    go [x] i = i
    go (s : v : ss) i
      | notThe s /= Nothing = go (v : ss) i
      | otherwise = go ss (if head v `elem` vowels then i + 1 else i)

-- §

-- §countVowels
countVowels :: String -> Int
countVowels s = length $ filter (`elem` vowels) s

-- §

-- §mkWord
newtype Word'
  = Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = if length s `div` 2 >= countVowels s then Just (Word' s) else Nothing

-- §

-- §naturals
data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Int
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Int -> Maybe Nat
integerToNat i
  | i == 0 = Just Zero
  | i > 0 = Just (Succ x)
  | otherwise = Nothing
  where
    (Just x) = integerToNat (i - 1)

-- §

-- §maybeLibrary
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just x) = f x
mayybee y _ Nothing = y

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just (head xs)

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs
  | length xs == length (catMaybes xs) = Just (catMaybes xs)
  | otherwise = Nothing

-- §

-- §eitherLibrary
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

extractLeft :: Either a b -> a
extractLeft (Left a) = a
extractLeft _ = error "Not a left item"

lefts' :: [Either a b] -> [a]
lefts' = foldr (\x xs -> if isLeft x then extractLeft x : xs else xs) []

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

extractRight :: Either a b -> b
extractRight (Right a) = a
extractRight _ = error "Not a right item"

rights' :: [Either a b] -> [b]
rights' = foldr (\x xs -> if isRight x then extractRight x : xs else xs) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

-- §

-- §catamorphisms
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
  case f b of
    Nothing -> []
    Just (a, b') -> a : myUnfoldr f b'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a))

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
  Nothing -> Leaf
  Just (a', b, a'') -> Node (unfold f a') b (unfold f a'')

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\i -> if i >= n then Nothing else Just (i + 1, i, i + 1)) 0

-- §
