rlencode0 :: [Int] -> [Int]
rlencode0 [] = []
rlencode0 (x:xs)
    | x == 0 = 0 : ehZero (x:xs) : rlencode0 (dropWhile (==0) xs)
    | otherwise = x : rlencode0 xs

rldecode0 :: [Int] -> [Int]
rldecode0 [] = []
rldecode0 (0:x:xs) = ehZero' x ++ rldecode0 xs
rldecode0 (x:xs) = x : rldecode0 xs

ehZero :: [Int] -> Int
ehZero [] = 0
ehZero (x:xs) = length (takeWhile (==0) (dropWhile (/=0) (x:xs)))

ehZero' :: Int -> [Int]
ehZero' 0 = []
ehZero' n = 0 : ehZero' (n-1)