display_asterisk :: Int -> IO ()
display_asterisk 0 = do putChar '\n'
display_asterisk n = do putStr "* "
                        display_asterisk (n-1)
check_win :: [Int] -> Bool
check_win xs = all (<=0) xs
update_board :: [Int] -> Int -> Int -> [Int]
update_board xs row asterisk = [if idx == row then x - asterisk else x | (idx,x) <- board] where board = zip [1..] xs 
initializing :: IO () 
initializing =  do putStrLn "Initializing..."
                   display_board 1 [5,4,3,2,1]
display_board ::Int ->[Int] -> IO ()
display_board  6 xs = return ()
display_board a (x:xs) = do putStr (show a)
                            putStr ": "
                            display_asterisk x
                            display_board (a+1 ) xs
player_one :: [Int] -> IO ()
player_one xs =  do putStrLn "Player one is playing"
                    putStrLn "Please enter a row"
                    x <- getLine
                    let a = (read x :: Int)
                    putStrLn "Please how many start"
                    y <-getLine
                    let b = (read y :: Int)
                    let new_s = update_board xs a b
                    if check_win (new_s) == True then putStrLn "Player 1 won" else  do 
                                                                                        display_board 1 (new_s)
                                                                                        player_two  (new_s)
player_two :: [Int] -> IO ()
player_two xs =  do putStrLn "Player two is playing"
                    putStrLn "Please enter a row"
                    x <- getLine
                    let a = (read x :: Int)
                    putStrLn "Please how many start"
                    y <-getLine
                    let b = (read y :: Int)
                    let new_s = update_board xs a b
                    if check_win (new_s) == True then putStrLn "Player 2 won" else  do 
                                                                                        display_board 1 (new_s)
                                                                                        player_two  (new_s)
main :: IO ()
main = do
            initializing
            player_one [5,4,3,2,1]
