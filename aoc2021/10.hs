import Utils
import Data.List
import Debug.Trace

type Acc = ([Char], Char)

rowReducer :: Acc -> Char -> Acc
rowReducer ([], c) check
  | check `elem` "<([{"       = ([check], c)
  | otherwise                 = ([], c)
rowReducer (stack@(x:xs), c) check
  | check `elem` "<([{"       = (check : stack, c)
  | check == '>' && x == '<'  = (xs, c)
  | check == ']' && x == '['  = (xs, c)
  | check == '}' && x == '{'  = (xs, c)
  | check == ')' && x == '('  = (xs, c)
  | otherwise                 = (xs, check)

scoreCorrupted :: Acc -> Int
scoreCorrupted (_, c)
  | c == ')'  = 3
  | c == ']'  = 57
  | c == '}'  = 1197
  | c == '>'  = 25137
  | otherwise = 0

scoreIncomplete :: Acc -> Int
scoreIncomplete (stack, c)
  | c /= ' '  = 0
  | otherwise = foldl stackReducer 0 stack
  where 
    stackReducer :: Int -> Char -> Int
    stackReducer acc x
      | x == '('  = 1 + 5 * acc
      | x == '['  = 2 + 5 * acc
      | x == '{'  = 3 + 5 * acc
      | x == '<'  = 4 + 5 * acc
      | otherwise = error "this shouldn't happen"
      

main = do
  rows <- readLines "10.dat"
  let res = map (foldl rowReducer ("", ' ')) rows
  print . sum $ map scoreCorrupted res
  let incompleteScores = sort . filter (/=0) $ map scoreIncomplete res
  print $ incompleteScores !! (length incompleteScores `div` 2)
