signum n | n < 0 = -1
         | n == 0 = 0
         | otherwise = 1
(&&) :: Bool -> Bool -> Bool
True && True = True
_    && _    = False
lastElement :: [a] -> a
lastElement [xs]   = xs
lastElement (_:xs) = lastElement xs 
odds n = map f [0..n-1]
         where
             f x = x*2 +1
odds_n n = map (\x -> 2*x +1) [0.. n-1]
