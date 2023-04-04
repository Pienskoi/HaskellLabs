-- Task 1.7
-- With Prelude functions
applyEveryNth :: Int -> (a -> a) -> [a] -> [a]
applyEveryNth _ _ [] = []
applyEveryNth 0 _ xs = xs
applyEveryNth 1 f xs = map f xs
applyEveryNth n f xs = case splitAt (n-1) xs of
    (pre, []) -> pre
    (pre, x:xs) -> pre ++ f x : applyEveryNth n f xs

-- Task 2.7
-- With Prelude functions
findPrimeNumbers :: Int -> Int -> [Int]
findPrimeNumbers a b = filterRange a b isPrime

isPrime :: Int -> Bool
isPrime n = n > 1 && null [ x | x <- [2..n `div` 2], n `mod` x == 0]

filterRange :: Int -> Int -> (Int -> Bool) -> [Int]
filterRange a b f
    | a < b = filter f [a..b]
    | otherwise = filter f [b..a]

