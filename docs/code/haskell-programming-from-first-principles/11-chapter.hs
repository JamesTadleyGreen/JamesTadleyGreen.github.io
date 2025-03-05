import Data.Char
import Data.List (elemIndex, intersperse)
import Debug.Trace

{- cabal:
build-depends: base, text, string-qq
-}

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

-- §treeMap
treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap _ Leaf = Leaf
treeMap f (Node left a right) = Node (treeMap f left) (f a) (treeMap f right)

-- §

-- §orders
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

-- §

-- §treeFold
treeFold :: (a -> b -> b) -> b -> BinaryTree a -> b
treeFold f b t = foldr f b (preorder t)

treeFold' :: (a -> b -> b) -> b -> BinaryTree a -> b
treeFold' f b (Node Leaf a Leaf) = f a b
treeFold' f b (Node left a Leaf) = treeFold' f (f a b) left
treeFold' f b (Node Leaf a right) = treeFold' f (f a b) right
treeFold' f b (Node left a right) = treeFold' f b' left
  where
    b' = treeFold' f (f a b) right

-- §

-- §vigenereCipher
type Key = String

type Message = String

vigenereCipher :: Key -> Message -> Message
vigenereCipher k m = map rotate $ zip (concat (repeat (k))) m
  where
    rotate (k, m) = chr $ (mod (ord k - 97 + ord m - 97) 26) + 97

-- §

-- §isSubsequenceOf
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf l@(y : ys) (x : xs)
  | y == x = isSubsequenceOf ys xs
  | otherwise = isSubsequenceOf l xs

-- §

-- §capitalizeWords
capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map (\w@(x : xs) -> (w, toUpper x : xs)) $ words s

-- §

-- §capitalizeWord
capitalizeWord :: String -> String
capitalizeWord (x : xs) = toUpper x : xs

-- §

-- §capitlizeParagraph
capitalizeParagraph :: String -> String
capitalizeParagraph s = capitalizeParagraph' $ words s

capitalizeParagraph' :: [String] -> String
capitalizeParagraph' [] = []
capitalizeParagraph' [x] = x
capitalizeParagraph' (x : y : xs)
  | last x == '.' = concat $ intersperse " " [x, capitalizeWord y, capitalizeParagraph' xs]
  | otherwise = x ++ " " ++ capitalizeParagraph' (y : xs)

-- §

-- §phoneExercise
type Digit = Char

type Presses = Int

data PhoneKey = PhoneKey Digit [Char]

data DaPhone = DaPhone [PhoneKey]

phone :: DaPhone
phone =
  DaPhone
    [ PhoneKey '1' "1",
      PhoneKey '2' "abc2",
      PhoneKey '3' "def3",
      PhoneKey '4' "ghi4",
      PhoneKey '5' "jkl5",
      PhoneKey '6' "mno6",
      PhoneKey '7' "pqrs7",
      PhoneKey '8' "tuv8",
      PhoneKey '9' "wxyz9",
      PhoneKey '0' " 0"
    ]

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"
  ]

reverseTaps' :: DaPhone -> Char -> (Digit, Presses)
reverseTaps' (DaPhone (PhoneKey k ks : keys)) x =
  let presses = elemIndex x ks
   in case presses of
        Nothing -> reverseTaps' (DaPhone keys) x
        Just p -> (k, p + 1)

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone x
  | isUpper x = [('*', 1), reverseTaps' phone (toLower x)]
  | otherwise = [reverseTaps' phone x]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone message = foldr ((++) . reverseTaps phone) [] message

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps taps = sum [p | (_, p) <- taps]

-- §

-- §huttonsRazor
data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Add x y) = eval x + eval y
eval (Lit x) = x

printExpr :: Expr -> String
printExpr (Add x y) = printExpr x ++ "+" ++ printExpr y
printExpr (Lit x) = show x

-- §
