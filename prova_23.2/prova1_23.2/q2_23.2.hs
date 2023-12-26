merginho :: Ord t => t -> [t] -> [t]
merginho x [] = [x]
merginho x (y:ys)
    | x <= y    = x : y : ys
    | otherwise = y : merginho x ys

merge :: Ord t => [t] -> [t] -> [t]
merge [] [] = []
merge [] (y:ys) = (y:ys)
merge (x:xs) [] = (x:xs)
merge (x:xs) (y:ys) = merginho x (merge xs (y:ys))