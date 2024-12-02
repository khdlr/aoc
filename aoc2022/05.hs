import Data.List
import Data.List.Split

parseLayout :: [String] -> [String]
parseLayout = map (>>= id) . transpose . map parseLayoutLine

parseLayoutLine = (map parseLayoutBlock) . chop (\a -> (take 3 a, drop 4 a))

parseLayoutBlock :: String -> [Char]
parseLayoutBlock ['[', a, ']'] = [a]
parseLayoutBlock _ = []

execute :: (Int, Int, Int) -> [String] -> [String]
execute (1, from, to) stacks = map ithStack [0..(length stacks)-1]
  where
    ithStack :: Int -> String
    ithStack i
      | i == from  = tail (stacks !! i)
      | i == to    = movedItem : (stacks !! i)
      | otherwise  = stacks !! i
    movedItem = head (stacks !! from)
execute (n, from, to) stacks = execute ((n-1), from, to) . execute (1, from, to) $ stacks

execute2 :: (Int, Int, Int) -> [String] -> [String]
execute2 (n, from, to) stacks = map ithStack [0..(length stacks)-1]
  where
    ithStack :: Int -> String
    ithStack i
      | i == from  = drop n (stacks !! i)
      | i == to    = movedItems ++ (stacks !! i)
      | otherwise  = stacks !! i
    movedItems = take n (stacks !! from)

parseCommand :: String -> (Int, Int, Int)
parseCommand = parseInts . words
  where
    parseInts ["move", idx, "from", from, "to", to] =
      (read idx, read from - 1, read to - 1)

main = do
  content <- readFile "05.dat"
  let 
    [init_raw, commands_raw] = splitOn "\n\n" content
    commands = map parseCommand . lines $ commands_raw
    init = parseLayout . lines $ init_raw

  print "Task 1"
  print . map head . foldl (flip execute) init $ commands
  
  print ""
  print "Task 2"
  print . map head . foldl (flip execute2) init $ commands
  -- print "asdf"

