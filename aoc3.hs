import System.IO
import System.Environment
import Data.List.Split

main = do
    handle <- openFile "input3.txt" ReadMode
    contents <- hGetContents handle 
    let rs = santa $ tail contents
        s = santa contents
        p = positions s (0,0)
    putStr $ show $ houses s 1 [(0, 0)] + houses rs 0 ((0,0):p) 
    --putStr $ show $ houses contents 1 [(0, 0)]
    hClose handle

direction :: Char -> (Int, Int)-> (Int, Int)
direction x (a, b) = case x of 
  '^' ->  (a, b+1)
  'v' ->  (a, b-1)
  '>' ->  (a+1, b)
  '<' ->  (a-1, b)
--  _   ->  (a, b)

houses :: [Char] -> Int -> [(Int, Int)] -> Int
houses [] h _ = h 
houses (x:xs) h as 
  | elem a as = houses xs h (a:as)
  | otherwise = houses xs (h+1) (a:as)
    where 
      a = direction x (head as)

santa :: [Char] -> [Char]
santa [] = []
santa [x] = [x]
santa (e1:e2:xs) = e1 : santa xs

positions :: [Char] -> (Int, Int)-> [(Int, Int)]
positions [] (a, b)  = [(a, b)]
positions (x:xs) (a, b) = direction x (a, b) : positions xs (direction x (a, b))
