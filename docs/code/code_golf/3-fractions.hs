{- cabal:
build-depends: base
-}

import Data.List

s a=map read $ words [if c=='/' then ' ' else c|c<-a]

args = ["10/5"]
-- import System.Environment
main = do
 mapM p j
p i = do
 let a = s i
 let g = gcd (a!!0) (a!!1)
 print(intercalate "/" $ map (show . (`div` g)) a)
