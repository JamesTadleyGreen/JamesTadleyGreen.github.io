---
title: "Part IV: Basic Datatypes"
tags: haskell,type
---

# Chapter Exercises
1.
    ```haskel
    length :: [a] -> Int

    ```

    `length` takes a list, of any type, and returns an `int`, representing the number of elements in the list.

1. 
    1. 5
    1. 3
    1. 2
    1. 5

1. The latter will error due to `length` returning an `int` instead of a `Fractional`
1. 
    ``Prelude> 6 `div` length [1, 2, 3]``
1. The type will be a `bool`, we'd expect a `True` result.
1. The type again will be a `bool`, this time `False`.
1.
    1. This will work, and will return `True`.
    1. This won't work as lists must be homogenous.
    1. This will return an `int`.
    1. This will return `False` since 'b' is not less than 'a'.
    1. This will error as Haskell does not have truthy vales.

1. 
    ```haskell
    isPalindrome :: (Eq a) => [a] -> Bool
    isPalindrome x = x == reverse x

    ```
1.
    ```haskell
    myAbs :: Integer -> Integer
    myAbs x = if x < 0 then -x else x

    ```

1. 
    ```haskell 
    f :: (a, b) -> (c, d) -> ((b, d), (a, c))
    f t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))
    f t1 t2 = map (snd, fst) (t1, t2)

    ```

# Reading Syntax
1. Replace quotes with backticks
1. Lowercase the variable definition
1. Put the list in brackets, e.g. `(x:xs)`
1. Lowercase the response, `a`

# Matching functions and types
1. c)
1. b)
1. a)
1. d)

a)
a)

