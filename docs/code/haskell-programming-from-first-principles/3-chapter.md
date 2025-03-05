---
title: "Part III: Strings"
tags: haskell,strings
---

# Chapter Exercises
## Reading Syntax
1.
    1. Ok
    1. `[1,2,3] ++ [4,5,6]`
    1. Ok
    1. Ok
    1. `"hello" !! 4`
    1. Ok
    1. `take 4 "lovely"`
    1. Ok

1.
    1. $a) \to d)$
    1. $b) \to c)$
    1. $c) \to e)$
    1. $d) \to a)$
    1. $e) \to b)$

## Building Functions
1. 
    1. 
        ```haskell
        exclaim :: String -> String
        exclaim = (++"!")
        ```
    1.
        ```haskell
        make_y :: String -> Char
        make_y = const 'y'
        make_y = (4!!)
        make_y = (last . head . words)
        ```
1. Done
1. 
    ```haskell
    thirdLetter :: String -> Char
    thirdLetter = (2!!)
   ```
1.
    ```haskell
    letterIndex :: Int -> String -> Char
    letterIndex = (!!)
    thirdletter = (letterIndex 2)

    ```

1. 
    ```haskell
    rvrs :: String -> String
    rvrs = unwords . rvrs' . words

    rvrs' :: [a] -> [a]
    rvrs' (x:xs) = rvrs' xs ++ [x]
    rvrs' [] = []
    ```
