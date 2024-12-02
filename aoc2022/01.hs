import Data.List

parse :: [String] -> [[Int]]
parse [x] = [[read x]]
parse (x : xs) = case x of 
  "" -> [] : parse xs
  _  -> (read x : head) : rest
    where head : rest = parse xs

main = do
  content <- readFile "01.dat"
  let readings = parse (lines content)
  let carry = map sum $ readings
  print ("task 1: " ++ (show . maximum $ carry))

  let sorted = sortBy (\a b -> compare b a) carry
  print ("task 2: " ++ (show . sum . take 3 $ sorted))
