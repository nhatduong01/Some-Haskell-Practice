searchL:: Integer -> [Integer] -> Integer
searchL x xs | head xs == x = 0
             | otherwise = 1 + (searchL x (tail xs))