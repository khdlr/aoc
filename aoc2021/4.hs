import Debug.Trace

readline :: [Char] -> [Int]
readline = map (\x -> read [x] :: Int)

split_or_append :: Char -> [[Char]] -> Char -> [[Char]]
split_or_append delim acc chr
  | delim == chr  = acc ++ [[]]
  | otherwise     = (init acc) ++ [(last acc) ++ [chr]]

split_str :: Char -> [Char] -> [[Char]]
split_str delim = foldl (split_or_append delim) [[]]

main = do
  text <- readFile "4.dat"
  let content = lines text
  let numbers = map (read :: [Char] -> Int) (split_str ',' (head content))
  print ("task 1: ")
  print numbers
