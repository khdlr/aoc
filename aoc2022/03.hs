import Data.List
import Data.List.Split
import Data.Char (ord)

parseRucksack :: String -> [[Char]]
parseRucksack line = [first, second]
  where first = take ((length line) `div` 2) line
        second = drop ((length line) `div` 2) line

commonItem :: [[Char]] -> Char
commonItem = head . (foldr1 intersect)

priority :: Char -> Int
priority c
  | num >= 97 = num - 96
  | num < 97  = num - (65 - 27)
  where num = ord c

main = do
  content <- readFile "03.dat"
  let rucksacks = map parseRucksack (lines content)
  print ("task 1: " ++ (show . sum . map (priority . commonItem) $ rucksacks))

  let groups = chunksOf 3 (lines content)
  print ("task 2: " ++ (show . sum . map (priority . commonItem) $ groups))

