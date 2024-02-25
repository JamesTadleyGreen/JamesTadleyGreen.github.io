-- \|OLS
import Statistics

data Stats =
  Stats
    { coef :: Float
    , stdErr :: Float
    , t :: Float
    , p :: Float
    }
  deriving (Show)

data Summary =
  Summary
    { intercept :: Stats
    , lstat :: Stats
    }
  deriving (Show)

mean :: [Float] -> Float
mean xs = sum xs / fromIntegral (length xs)

ols :: [Float] -> [Float] -> Summary
ols xs ys =
  Summary
    { intercept = Stats {coef = beta0, stdErr = sEBeta0, t = tBeta0, p = pBeta0}
    , lstat = Stats {coef = beta1, stdErr = sEBeta1, t = tBeta1, p = pBeta1}
    }
  where
    n = fromIntegral $ length xs :: Float
    xBar = mean xs
    yBar = mean ys
    eXY = foldr ((+) . \(x, y) -> (x - xBar) * (y - yBar)) 0 (zip xs ys)
    eX2 = foldr ((+) . \x -> (x - xBar) ^ 2) 0 xs
    beta1 = eXY / eX2
    beta0 = yBar - beta1 * xBar
    rss = foldr ((+) . \(x, y) -> (y - beta0 - beta1 * x) ^ 2) 0 (zip xs ys)
    rse2 = rss / (n - 2)
    sEBeta1 = rss / eX2
    sEBeta0 = rse2 * (1 / n + xBar ^ 2 / eX2)
    tBeta0 = beta0 / sEBeta0
    tBeta1 = beta1 / sEBeta1
    pBeta0 = wilcoxonMatchedPairSignificance n beta0
    pBeta1 = wilcoxonMatchedPairSignificance n beta1

main :: IO ()
main = print $ ols [1, 3, 5] [4.8, 12.4, 15.5]
