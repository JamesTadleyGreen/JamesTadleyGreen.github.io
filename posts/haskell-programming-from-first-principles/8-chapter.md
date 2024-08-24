---
title: "Part VII: Functional Patterns"
tags: haskell,polymorphic,patterns
---

# Chapter Exercises
1. d)
1. b)
1. d)
1. b)

# Reviewing Currying
1. `"woops mrow woohoo!"`
1. `"1 mrow haha"`
1.  `"woops mrow 2 mrow haha"`
1. Okay I'm bored now

# Recursion
1. 
    ```haskell
    dividedBy 15 2 =
        go 15 2 0 =
        go 13 2 1 = 
        go 11 2 2 = 
        go 9  2 3 =
        go 7  2 4 =
        go 5  2 5 =
        go 3  2 6 =
        go 1  2 7 = (7, 1)
        
    ```
1. 
    ```haskell
    cumSum :: (Eq a, Num a) => a -> a
    cumSum 0 = 0
    cumSum n = n + cumSum (n - 1)

    ```

1. 
    ```haskell
    stupidProduct :: (Integral a) => a -> a -> a
    stupidProduct 0 _ = 0
    stupidProduct a b = b + stupidProduct (a - 1) b

    ```


