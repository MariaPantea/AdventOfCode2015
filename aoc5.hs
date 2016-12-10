import Data.List

vowels = "aeiou"
nono = ["ab", "cd", "pq", "xy"]

hasThreeVowels :: Int -> String ->  Bool
hasThreeVowels i []     = i >= 3
hasThreeVowels 3 as     = True
hasThreeVowels i (a:as) = if elem a vowels 
                          then hasThreeVowels (i+1) as
                          else hasThreeVowels i as

hasLetterTwice :: String -> Bool
hasLetterTwice [] = False
hasLetterTwice [a] = False
hasLetterTwice (a:as) = a == head as || hasLetterTwice as

hasNonoStrings :: [String] -> String -> Bool
hasNonoStrings [] as  = False
hasNonoStrings nos as = isInfixOf (head nos) as || hasNonoStrings (tail nos) as

-- Part 2



main :: IO ()
main = do
  file <- readFile "input5.txt"
  s <- return $ lines file
  s' <- return $ filter (hasThreeVowels 0) $ filter hasLetterTwice s
  print $ length $ filter (==False) $ map (hasNonoStrings nono) s'
-- part 2

