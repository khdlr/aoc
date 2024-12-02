import Utils
import Debug.Trace

initList :: [Int] -> [Int]
initList xs =
  map count [0..n]
  where
    count x = length $ filter (==x) xs
    n = 1 + maximum xs

costToMove :: (Int -> Int) -> Int -> (Int, Int) -> Int
costToMove costFun tgt (src, crabcount) = crabcount * costFun(abs(src - tgt))

costTo :: (Int -> Int) -> [Int] -> Int -> Int
costTo costFun crabs n = sum (map (costToMove costFun n) (zip [0..] crabs))

solveTask :: (Int -> Int) -> [Int] -> IO Int
solveTask costFun crabs = do
  let counts = initList crabs
  let costs = map (costTo costFun counts) [0..(1 + maximum crabs)]
  let best = minimum costs
  putStrLn . show $ best
  return best

main = do
  crabs <- readCommaList "7.dat"
  -- let crabs = [16,1,2,0,4,2,7,1,2,14]

  solveTask id crabs
  solveTask (\x -> (x * (x+1)) `div` 2) crabs

  putStrLn . show . round $ (fromIntegral $ sum crabs) / (fromIntegral $ length crabs)
