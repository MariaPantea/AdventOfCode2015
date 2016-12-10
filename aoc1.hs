import System.Environment  
import System.IO  


main = do  
    handle <- openFile "input.txt" ReadMode  
    contents <- hGetContents handle  
    putStr . show $ basement contents 0 0
    hClose handle

    

counter :: [Char] -> Int
counter xs = foldl (\a x -> if x == '(' then a+1 else a-1) 0 xs

move :: Char -> Int
move c = if (c == '(') then 1 else -1

basement :: [Char] -> Int -> Int -> Int
basement _ (-1) b = b
basement (x:xs) cf b = basement xs (cf + move x) (b + 1)
