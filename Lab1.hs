-- Task 1.7
-- With Prelude functions
deleteEveryNth :: Int -> [a] -> [a]
deleteEveryNth _ [] = []
deleteEveryNth 0 xs = xs
deleteEveryNth 1 xs = []
deleteEveryNth n xs = case splitAt (n-1) xs of
    (pre, []) -> pre
    (pre, _:xs) -> pre ++ deleteEveryNth n xs

-- Task 2.7
-- With Prelude functions
findFirstPrime :: Int -> Int -> Int
findFirstPrime a b
    | a > b = findFirstPrime b a
    | isPrime a = a  
    | a == b = error "No prime number in range"
    | otherwise = findFirstPrime (a+1) b
    where isPrime n = null [ x | x <- [2..n-1], n `mod` x == 0]

