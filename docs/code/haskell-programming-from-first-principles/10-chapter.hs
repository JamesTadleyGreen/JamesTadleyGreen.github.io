{- cabal:
build-depends: base, text, string-qq
-}
-- §stopVowelStop
stopVowelStop :: String -> String -> [(Char, Char, Char)]
stopVowelStop s1 s2 = [(a, b, c) | a <- s1, b <- s2, c <- s1]

stopVowelStopP :: String -> String -> [(Char, Char, Char)]
stopVowelStopP s1 s2 = filter (\(a, _, _) -> a == 'p') $ stopVowelStop s1 s2

-- §

-- §seekritFunc
seekritFunc :: (Fractional a) => String -> a
seekritFunc x = (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))

-- §
