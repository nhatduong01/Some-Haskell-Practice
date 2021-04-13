and :: [Bool] ->Bool
and [] = True
and (x:xs) | x == True = Main.and xs
           | otherwise = False
concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ Main.concat xs 
replicate :: Int -> a ->[a]
replicate 0 a = []
replicate n a = [a] ++ Main.replicate (n-1) a 
(!!):: [a] -> Int -> a
(!!) a x  | x == 0    = head a
          | otherwise = (Main.!!) (tail a) (x-1)
elem :: Eq a => a -> [a] ->Bool
elem a xs | xs == [] = False
          | a == head xs = True
          | otherwise = Main.elem a (tail xs)
{-
Define a recursive funciton that merges two sorted lists of values
to give single sorted list
-}
merge :: Ord a => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge a b  = if (head a) > head (b) then [head b] ++ merge a (tail b)
              else [head a] ++ merge (tail a) b
msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge (msort ( take half xs)) (msort (drop half xs )) where half =  length xs `div` 2
