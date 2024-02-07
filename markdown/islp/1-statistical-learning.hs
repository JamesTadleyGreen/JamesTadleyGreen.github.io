-- \|K-Nearest Neighbours
import Data.List (group, maximumBy, sort, sortBy)
import Data.Ord (comparing)

-- https://stackoverflow.com/questions/21354997/compute-the-mode-of-a-list
mode :: (Ord a) => [a] -> a
mode = head . maximumBy (comparing length) . group . sort

knn :: (Num a, Ord b, Ord c) => ([a] -> [a] -> b) -> Int -> [[a]] -> [c] -> [a] -> c
knn measure k xs y x = mode $ take k orderedPredictions
  where
    orderedPredictions = map fst $ sortBy (comparing snd) distances
    distances = zip y (map (measure x) xs)

euclidianDistance :: [Float] -> [Float] -> Float
euclidianDistance x y = sqrt $ foldr ((+) . (\(x, y) -> x ^ 2 + y ^ 2)) 0 (zip x y)

main :: IO ()
main = print $ knn euclidianDistance 3 [[0, 3, 0], [2, 0, 0], [0, 1, 3], [0, 1, 2], [-1, 0, 1], [1, 1, 1]] ["Red", "Red", "Red", "Green", "Green", "Red"] [0, 0, 0]
