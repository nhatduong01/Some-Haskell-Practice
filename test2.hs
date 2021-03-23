print_number:: Int -> Int
print_number n 0 = return ()
print_number n m =
       do
        print (n)
        print_number n (m-1)
f :: Int -> [Int] -> [Int]
f n arr =
       do
        print_number n arr!!0
        f n drop 1 arr
f n null  = return ()


-- This part handles the Input and Output and can be used as it is. Do not modify this part.
main :: IO ()
main = getContents >>=
       mapM_ print. (\(n:arr) -> f n arr). map read. words