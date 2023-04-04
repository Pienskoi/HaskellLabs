-- Task 1.7
-- Without Prelude functions
deleteEveryNth' :: Int -> [a] -> [a]
deleteEveryNth' _ [] = []
deleteEveryNth' 0 xs = xs
deleteEveryNth' 1 xs = []
deleteEveryNth' n xs = deleteEveryNth'' n xs
    where deleteEveryNth'' 1 (x:xs) = deleteEveryNth'' n xs
          deleteEveryNth'' counter (x:xs) = x : deleteEveryNth'' (counter-1) xs

-- Task 2.7
-- Without Prelude functions
findFirstPrime' :: Int -> Int -> Int
findFirstPrime' a b
    | a > b = findFirstPrime' b a
    | isPrime a = a  
    | a == b = error "No prime number in range"
    | True = findFirstPrime' (a+1) b
    where
        isPrime n = [ x | x <- [2..n-1], isDivisible n x] == []
        isDivisible x y
            | x == y = True
            | x < y = False
            | True = isDivisible (x-y) y

