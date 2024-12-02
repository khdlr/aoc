import Data.Bifunctor (second)
import Data.Foldable (foldr', foldl')
import Data.List (elemIndex)
import Debug.Trace

readData :: IO [Int]
readData = do
  content <- readFile "20.dat"
  return . map read . lines $ content

splice :: Int -> [Int] -> ([Int], Int, [Int])
splice 0 (x:xs) = ([], x, xs)
splice n (x:xs) = (x:a, b, c)
  where (a, b, c) = splice (n-1) xs

insert :: Int -> Int -> [Int] -> [Int]
insert item position prev = before ++ [item] ++ after
  where
    (before, after) = splitAt position prev

mix :: [Int] -> Int -> Int -> [Int]
mix indices pos amount = insert item newPosition (before ++ after)
  where
    newPosition = (pos + amount) `mod` (length indices - 1)
    (before, item, after) = splice pos indices

index :: Eq a => a -> [a] -> Int
index x xs = case elemIndex x xs of Just a -> a

main = do
  numbers <- readData

  let
    n = length numbers
    actions = zip [0..] numbers

    apply :: [Int] -> (Int, Int) -> [Int]
    apply prev (idx, amt) = mix prev (index idx prev) amt

    st = foldl' apply [0..n-1] actions
    nums = map (numbers!!) st
    start = 0 `index` nums

  print . sum . map (\i -> nums !! ((start + i) `mod` n)) $ [1000, 2000, 3000]

  putStrLn "Part 2"
  let 
    mul = (811589153*)
    actions' = concat . replicate 10 . map (second mul) $ actions
    st' = foldl' apply [0..n-1] actions'
    nums' = map (mul . (numbers!!)) st'
    start' = 0 `index` nums'
  print . sum . map (\i -> nums' !! ((start' + i) `mod` n)) $ [1000, 2000, 3000]
