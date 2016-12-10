import System.IO
import System.Environment
import Data.List.Split

main = do
    handle <- openFile "input2.txt" ReadMode
    contents <- hGetContents handle 
   -- putStr $ show $ foldl1 (+) $ map wrapper $ map splitList (lines contents)
    putStr $ show $ foldl1 (+) $ map ribbon $ map splitList (lines contents)
    hClose handle

splitList :: String -> [Int] 
splitList xs = map read (splitOn "x" xs)


wrapper :: [Int] -> Int 
wrapper xs = 2*l*w + 2*w*h + 2*h*l + minimum [l*w, w*h, h*l]
  where 
    w = (xs!!0) :: Int
    l = (xs!!1) :: Int
    h = (xs!!2) :: Int

ribbon :: [Int] -> Int
ribbon xs
  | w >= l && w >= h = w*l*h + 2*l + 2*h
  | l >= w && l >= h = w*l*h + 2*w + 2*h
  | h >= l && h >= w = w*l*h + 2*l + 2*w
    where 
      w = (xs!!0) :: Int
      l = (xs!!1) :: Int
      h = (xs!!2) :: Int