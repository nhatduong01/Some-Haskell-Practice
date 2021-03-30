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
len :: [a] -> Int
len [] = 0
len lst = 1 + len (tail lst)
{-
Update the values of a list with their absolute values. 
The input and output portions will be handled automatically during grading. 
You only need to write a function with the recommended method signature.
-}
array_abs [] = []
array_abs lst = [abs (head lst)] ++ array_abs (tail lst)
{-
The series expansion of e**x is given by:
1 + x + x^2/2! + x^3/3!
-}
factorial :: Float -> Float
factorial 0 = 1
factorial n = n * factorial (n-1)
evaluate_e :: Float -> Float
evaluate_e n = 1 + n + (n**2)/(factorial 2)  + (n**3)/(factorial 3) + (n**4)/(factorial 4) + 
               (n**5)/(factorial 5) + (n**6)/(factorial 6) + (n**7)/(factorial 7) +(n**8)/(factorial 8) + (n**9)/(factorial 9)
{-
Area Under Curves and Volume of Revolving a Curve
This relates to definite integration via numerical methods.
Consider the algebraic expression given by:
(a1)x^(b1) +(a2)x^(b2) + (a3)x^(b3) + ... + (an)x^(bn)
1. Evaluate the area bounded by a given polynomial function of the kind described above, between the given limits of 
    L and R
2. Evaluate the volume of the solid obtained by revolving this polynomial curve around the x-axis
-}
-- This function should return a list [area, volume].
equation_square_coefficent :: [Double] -> [Double]
equation_square_coefficent  a  = [x*y| x <- a, y <- a]  
equation_square_power :: [Double] -> [Double]
equation_square_power a = [x + y | x <- a, y <- a]
equation :: [Double] -> [Double] -> Double -> Double
equation b c d =  sum [x*(d**y)  | (x,y) <- zip b c  ]
list_of_values:: Double -> Double -> [Double]
list_of_values a b = [a + 0.001*i | i <- [1..(b-a)/0.001] ]
solve :: Double -> Double -> [Double] -> [Double] -> [Double]
solve l r a b =  [sum [(equation a b value)*0.001 | value <- list_of_values l r ],
                  pi *sum [(equation (equation_square_coefficent a) (equation_square_power b) value)*0.001 | value <- list_of_values l r ]]


