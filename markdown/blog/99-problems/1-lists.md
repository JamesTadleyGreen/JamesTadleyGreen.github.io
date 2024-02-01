# Lists I
## Solution 1
I think making this a total function is wiser than the solutions given.
```haskell
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast xs = Just $ foldr1 (const id) xs
```

## Solution 2
I think they missed a couple of cases in their solution that would run-time error
```haskell
myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast [_] = Nothing
myButLast [x, _] = Just x
myButLast (x:xs) = myButLast xs
```

## Solution 3
In reality I'd do the below.
```haskell
elementAt :: [a] -> Int -> a
elementAt = (!!)
```

But for the sake of taking part I'll do this instead, additionally
this includes the `Maybe` type.
```haskell
elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt xs 0 = Just $ head xs
elementAt (x:xs) i = elementAt xs (i-1)
```
Whoops! I didn't read it should be 1-indexed. Oh well, I'm leaving it
as is.

## Solution 4
```haskell
myLength :: [a] -> Int
myLength = foldr (\_ x -> x + 1) 0
```

## Solution 5
This feels inefficient, I think `++` has poor performance since Haskell
uses linked lists.

```haskell
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [a] = [a]
myReverse (x:xs) = myReverse xs ++ [x]
```

## Solution 6

```haskell
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs
```

## Solution 7
## Solution 8
The second solution `''` came after looking at the solutions, I find it clearer this
way.
```haskell
compress' :: Eq a => [a] -> a -> [a]
compress' [] x = [x]
compress' xs x
  | head xs == x = xs
  | otherwise = x : xs

compress :: Eq a => [a] -> [a]
compress xs = reverse $ foldl compress' [] xs

compress'' :: Eq a => [a] -> [a]
compress'' (x:y:xs)
  | x == y = compress'' (y : xs)
  | otherwise = x : compress'' (y : xs)
compress'' xs = xs
```
