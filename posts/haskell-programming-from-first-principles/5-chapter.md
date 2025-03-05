---
title: "Part V: Types"
tags: haskell,type
---

# Chapter Exercises
1. c)
1. a)
1. b)
1. c)

# Determine the Type
1.
    a) `Int` actually `Num a => a`
    a) `Num a => (a, String)`
    a) `(Int, String)`
    a) `Bool`
    a) `Int`
    a) `Bool`
1. `Num a => a`
1. `Num a => a -> a`
1. `Fractional a => a`
1. `String`

# Does it compile?
1. No, we're applying a numeric `10` to another `5^10`
1. This works, but is criminal
1. Won't work, applying two numeric's together again, not sure what they were going for
here
1. No c) defined.

# Type variable or constructor
1. Done for me 
1. Fully polymorphic, concrete x2
1. Fully polymorphic, constrained polymorphic, concrete
1. Fully polymorphic x2, concrete

# Type signatures
1. `functionH :: [a] -> a`
1. `Ord a => a -> a -> Bool`
1. `(a, b) -> b`

# Write a function
1. `id`
1. `f a b = a`
1. Yes.
1. `f a b = b`
1. `init` or `tail`
1. `(.)`
1. `f a b = b`
1. `f g a = g a`

# Fix it
1. 
    ```haskell
    module sing where

    fstString :: String -> String
    fstString x = x ++ " in the rain"

    sndString :: String -> String
    sndString x = x ++ " over the rainbow"

    sing :: String -> String -> String
    sing x y = if x > y then fstString x else sndString y

    main :: IO ()
    main = print $ sing "Somewhere" "Singing"
    ```

1. Reverse the inequality
1. 
    ```haskell
    module Arith3Broken where

    main :: IO ()
    main = do
    print $ 1 + 2
    putStrLn $ show 10
    print $ negate (-1)
    print $ (+) 0 blah
      where blah = negate 1
    ```

# Type-Kwon-Do
1. `h = f . g`
1. `e = w . q`
1. `xform (x, y) = (xz x, yz y)`
1. `munge f g x = fst $ g (f x)`
