{-
A triple (x, y, z) of positive integers is called pythagorean if x^2 + y^2 = z^2.
Using a list comprehension, define a function
-}
pyth:: Int->Int->Int->Bool
pyth a b c = if a^2 + b^2 == c^2 then True else False
pyths :: Int -> [(Int,Int,Int)]
pyths n =  [(x,y,z) | x <- [1..n], y <- [1..n], z <- [max x y ..n] , pyth x y z]
{-
a positive integer is perfect if it equals the sum of all of its factors, excluding the number ifself.
Using a list comprehension, define a function
-}
factors :: Int -> [Int]
factors n = [x | x <-[1..(n`div`2)], n `mod` x == 0]
perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], sum (factors x) == x]