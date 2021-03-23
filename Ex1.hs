doubleAll :: [Int] -> [Int]
doubleAll xs  = map (\x -> x*2) xs
searchL:: Integer -> [Integer] -> Integer
searchL x xs | head xs == x = 0
             | otherwise = 1 + (searchL x (tail xs))
positives :: [Integer] -> [Integer]
-- We assume 0 is included
positives [xs] = if xs >= 0 then [xs] else []
positives (x:xs) = if x >= 0 then [x] ++ (positives xs) else positives xs