# Sort

## Bubble Sort
This felt more difficult, I think due to how I think in Haskell the immutability
has really thrown me here. After an embarrassingly long time fighting here, I achieved
the below.
```haskell
bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = (iterate bubbleSort' xs) !! ((n * (n+1)) `div` 2)
    where n = length xs

bubbleSort' :: Ord a => [a] -> [a]
bubbleSort' [] = []
bubbleSort' [x] = [x]
bubbleSort' (x:y:xs)
    | x > y     = y : bubbleSort(x:xs)
    | otherwise = x : bubbleSort(y:xs)
```

### Notes
Wow this got hard fast, [this answer](https://codereview.stackexchange.com/questions/197868/bubble-sort-in-haskell)
shows that fold might have been the way to go, but to me it's less readable.
