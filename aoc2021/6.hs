import Utils
import Debug.Trace

initList :: [Int] -> [Int]
initList xs =
  map count [0..8]
  where
    count x = length $ filter (==x) xs

newPop :: [Int] -> Int -> Int
newPop pop n
  | n == 6    = pop !! 0 + pop !! 7
  | n == 8    = pop !! 0
  | otherwise = pop !! (n+1)

listStep :: [Int] -> [Int]
listStep pop = map (newPop pop) [0..8] 


main = do
  fishes <- readCommaList "6.dat"
  let state = initList fishes
  let states = iterate listStep state
  let fishAfter n = putStrLn . show . sum $ (states !! n)
  fishAfter 80
  fishAfter 256
