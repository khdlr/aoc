import Debug.Trace

readline :: [Char] -> [Int]
readline = map (\x -> read [x] :: Int)

count_bit :: (Int, Int) -> Int -> (Int, Int)
count_bit (zeros, ones) entry
  | entry == 0 = (zeros+1, ones)
  | entry == 1 = (zeros, ones+1)

count_bits :: [(Int, Int)] -> [Int] -> [(Int, Int)]
count_bits acc entry = map (uncurry count_bit) (zip acc entry)

gamma :: (Int, Int) -> Int
gamma (zeros, ones)
  | zeros > ones = 0
  | ones >= zeros = 1

epsilon :: (Int, Int) -> Int
epsilon (zeros, ones)
  | zeros <= ones = 0
  | ones < zeros = 1

to_decimal :: [Int] -> Int
to_decimal = foldl (\acc x -> 2*acc + x) 0

reduce_list :: ((Int, Int) -> Int) -> [[Int]] -> [Int]
reduce_list reduce_fn bits =
  map reduce_fn counts
  where
      init = map (\x -> (0, 0)) (bits !! 0)
      counts = (foldl count_bits init bits)

decode :: ((Int, Int) -> Int) -> [[Int]] -> [Int]
decode reduce_fn bits
  | length (bits !! 0) == 0 = []
  | otherwise = [bit] ++ decode reduce_fn reduced_bits
    where
      counts = foldl count_bits init bits
      init = map (\x -> (0, 0)) (bits !! 0)
      bit = (map reduce_fn counts) !! 0
      reduced_bits = (map tail) . (filter (\x -> x !! 0 == bit)) $ bits

most_common :: [Int] -> Int
most_common bits
  | zeros == 0    = 1
  | ones  == 0    = 0
  | zeros > ones  = 0
  | ones >= zeros = 1
  where
    (zeros, ones) = (foldl count_bit (0, 0) bits)

least_common :: [Int] -> Int
least_common bits
  -- | zeros == 0    = 1
  -- | ones  == 0    = 0
  | zeros > ones  = 1
  | ones >= zeros = 0
  where
    (zeros, ones) = (foldl count_bit (0, 0) bits)

part_two :: ([Int] -> Int) -> [[Int]] -> [Int]
part_two _ [x] = x
part_two criterion bits =
  [selector] ++ (part_two criterion truncated_bits)
  where
    truncated_bits = map tail filtered_bits
    filtered_bits  = filter (\x -> x !! 0 == selector) bits
    selector       = criterion (map (!! 0) bits)

main = do
  content <- readFile "3.dat"
  let bits = map readline (lines content)
  let g = to_decimal (reduce_list gamma bits)
  let e = to_decimal (reduce_list epsilon bits)
  print ("task 1: " ++ show (e * g))
  
  let oxygen = to_decimal (part_two most_common  bits)
  let co2    = to_decimal (part_two least_common bits)

  print oxygen
  print co2
  print ("task 2: " ++ show (oxygen * co2))
