-- |K-Nearest Neighbours
import Data.Vector
import Data.Csv

knn :: (Numeric a, Ord b) => (Vector a -> Vector a -> b) -> Int -> [Vector a] -> Vector c -> Vector a -> c
knn measure k xs y x = mode $ take k orderedPredictions
    where orderedPredictions = map fst $ sortBy (compare . snd) distances
          distances          = zip y (map (measure x) xs)

main :: IO()
main = print $ csvToKnn "./knn.csv" (Data.Vector.replicate 3 0)
