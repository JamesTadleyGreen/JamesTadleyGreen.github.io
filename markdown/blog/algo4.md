# Recursion
This feels like where Haskell would shine, it's much more integrated to the language
than for an imperative language.

## Maze
My gut instinct for how to first approach this is something of the form below.
```haskell
import Data.List

newtype Maze =
  Maze [String]

newtype Position =
  Position (Int, Int)

type Start = Position

type End = Position

newtype Path =
  Path [(Int, Int)]

instance Show Path where
  show (Path xs) =
    "<Path | " ++ intercalate ", " (map (\(x, y) -> show x ++ show y) xs) ++ ">"

solve :: Maze -> Start -> End -> Maybe Path
solve m s e = undefined

walk :: Maze -> Position -> Maybe Path
walk m p = undefined
```

Now we have the 'simple' task of defining what `solve` and `walk` look like.
