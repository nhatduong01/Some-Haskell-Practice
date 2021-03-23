f :: Int -> [Int] -> [Int]
f n [arr] = replicate n arr
f n (x:xs) = replicate n x ++ f n xs
-- Delimiter haskell
delimiter :: Int -> [Int] -> [Int]
delimiter n [xs] = if xs < n then [xs] else []
delimiter n (x:xs) = if x < n then [x] ++ delimiter n xs else delimiter n xs
{-
For a given list with N  integers, return a new list removing the elements at odd positions. 
The input and output portions will be handled automatically. 
You need to write a function with the recommended method signature.
-}
odd_ex :: Int ->[Int] ->[Int]
odd_ex n [arr] = if  n `mod` 2 == 1 then [] else [arr]
odd_ex n (x:xs) = if n `mod` 2 == 1 then odd_ex (n+1) xs else [x] ++ odd_ex (n+1) xs
odd_pos :: [Int] -> [Int]
odd_pos lst = odd_ex 0 lst
{-
Create an array of n  integers, where the value of n is passed as an argument to the pre-filled function in your editor. 
This challenge uses a custom checker, so you can create any array of  integers.
-}
array n = [x | x <-[1..n]]
{-
You are given a list of N elements. 
Reverse the list without using the reverse function. 
The input and output portions will be handled automatically. 
You need to write a function with the recommended method signature.
-}
rev :: [Int] -> [Int]
rev [] = []
rev (x:xs) = rev xs ++ [x] 
{-
Sum of the odd num given in a list
-}
sum_odd [] = 0
sum_odd f = if (head f) `mod` 2 == 1 then head f + sum_odd (tail f) else sum_odd (tail f)
{-
Count the number of elements in an array without using count, size or length operators (or their equivalents). 
The input and output portions will be handled automatically by the grader. 
You only need to write a function with the recommended method signature.
-}