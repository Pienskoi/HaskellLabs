-- Task 1.7
-- Without Prelude functions
applyEveryNth :: Int -> (a -> a) -> [a] -> [a]
applyEveryNth _ _ [] = []
applyEveryNth 0 _ xs = xs
applyEveryNth n f xs = applyEveryNth' n xs
    where applyEveryNth' 1 (x:xs) = f x : applyEveryNth' n xs
          applyEveryNth' _ [] = []
          applyEveryNth' counter (x:xs) = x : applyEveryNth' (counter-1) xs

-- Task 2.7
-- Without Prelude functions
findPrimeNumbers :: Int -> Int -> [Int]
findPrimeNumbers a b = filterRange a b isPrime

filterRange :: Int -> Int -> (Int -> Bool) -> [Int]
filterRange a b f
    | a > b = filterRange b a f
    | otherwise = [ x | x <- [a..b], f x]

isPrime :: Int -> Bool
isPrime n = n > 1 && [ x | x <- [2..n `div` 2], isDivisible n x] == []
    where isDivisible x y
            | x == y = True
            | x < y = False
            | True = isDivisible (x-y) y

