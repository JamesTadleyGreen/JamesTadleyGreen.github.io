---
title: "Part VII: Functional Patterns"
tags: haskell,polymorphic,patterns
---

# Chapter Exercises
1. d)
1. b)
1. d)
1. b)
1. a)

# Let's write code
1. 
    a.
        ```haskell
        getDigit :: Int -> Int -> Int
        getDigit 0 n = n `mod` 10
        getDigit i n = getDigit (i-1) $ n `div` 10

        tensDigit = getDigit 1
        ```
    a. Yes, 'cause I didn't use it

    a. `hunsDigit = getDigit 2`{.haskell}

1. 
    ```haskell
    foldBool1 :: a -> a -> Bool -> a
    foldBool1 x y b = 
      case b of
        True  -> x
        False -> y

    foldBool2 :: a -> a -> Bool -> a
    foldBool2 x y b
      | b = x
      | otherwise = y


    foldBool3 :: a -> a -> Bool -> a
    foldBool3 x _ True = x
    foldBool3 _ y False = y
    ```
1.
    ```haskell
    g :: (a -> b) -> (a, c) -> (b, c)
    g f (a, c) = (f a, c)

    ```

1. Nothing to do
1. `roundTrip = read . show`{.haskell}
1. `roundTrip x :: b`
