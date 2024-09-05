import Data.Array (Array, accum, elems, listArray, (//))     Ôûá Error when callin
import Data.Bits (FiniteBits (countTrailingZeros))
import Data.Hashable (Hashable (hash))
import Data.List
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import System.Random

phi :: Float
phi = 0.77351

randomList :: (Int, Int) -> StdGen -> Int -> [Int]
randomList i g n = take n $ randomRs i g

deterministicCount :: [Int] -> Int
deterministicCount = length . nub

