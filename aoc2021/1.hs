count_increases :: Int -> [Int] -> Int
count_increases n (x : xs) 
  | length xs < n     = 0
  | x < (xs !! (n-1)) = 1 + rest
  | otherwise         = rest
  where rest = count_increases_n n xs

main = do
  content <- readFile "1.dat"
  let readings = map read (lines content) :: [Int]
  let res1 = show . (count_increases 1) $ readings
  print ("task 1: " ++ res1)
  let res2 = show . (count_increases 3) $ readings
  print ("task 2: " ++ res2)
