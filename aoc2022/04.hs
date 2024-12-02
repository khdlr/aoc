import Data.List
import Data.List.Split
import Data.Char (ord)

toRange :: [Int] -> [Int]
toRange [a, b] = [a..b]

contained :: [[Int]] -> Bool
contained [[l1, l2], [r1, r2]] =
  (l1 <= r1 && l2 >= r2) ||
  (r1 <= l1 && r2 >= l2)

overlaps :: [[Int]] -> Bool
overlaps [[l1, l2], [r1, r2]] =
  (l1 >= r1 && l1 <= r2) ||
  (l2 >= r1 && l2 <= r2) ||
  (r1 >= l1 && r1 <= l2) ||
  (r2 >= l1 && r2 <= l2)

parseLine :: String -> [[Int]]
parseLine line = map ((map read) . (splitOn "-")) (splitOn "," line)

main = do
  content <- readFile "04.dat"
  let ranges = (map parseLine) . lines $ content;
  print (show . (length . (filter id)) . (map contained) $ ranges)

  print (show . (length . (filter id)) . (map overlaps) $ ranges)

