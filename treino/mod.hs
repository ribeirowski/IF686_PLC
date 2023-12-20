mod5 :: [Int] -> [Int]
mod5 [] = []
mod5 (x:xs) = x `mod` 5 : mod5 xs