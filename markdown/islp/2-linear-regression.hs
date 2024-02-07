import Debug.Trace (trace)
-- \|OLS

data Stats = Stats {coef :: Float, stdErr :: Float, t :: Float, p :: Float} deriving (Show)
data Summary = Summary {intercept :: Stats, lstat :: Stats} deriving (Show)

mean :: [Float] -> Float
mean xs = sum xs / fromIntegral (length xs)

ols :: [Float] -> [Float] -> Summary
ols xs ys = Summary{intercept = Stats{coef = beta0, stdErr = 0, t = 0, p = 0}, lstat = Stats{coef = beta1, stdErr = 0, t = 0, p = 0}}
  where
    xBar = mean xs
    yBar = mean ys
    eXY = foldr ((+) . \(x, y) -> (x - xBar) * (y - yBar)) 0 (zip xs ys)
    eX2 = foldr ((+) . \x -> (x - xBar) ^ 2) 0 xs
    beta1 = eXY / eX2
    beta0 = yBar - beta1 * xBar

main :: IO()
main = print $ ols [1,3,5] [4.8, 12.4, 15.5]
