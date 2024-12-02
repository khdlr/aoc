parse_command :: String -> (Int, Int)
parse_command line
  | cmd == "forward"  = (distance,  0)
  | cmd == "up"       = (0, -distance)
  | cmd == "down"     = (0,  distance)
  where
    [cmd, distance_str] = words line
    distance = (read distance_str) :: Int

tuple_add :: (Int, Int) -> (Int, Int) -> (Int, Int)
tuple_add (a, b) (c, d) = (a+c, b+d)

apply_command :: (Int, Int, Int) -> (Int, Int) -> (Int, Int, Int)
apply_command (x, y, aim) (dx, dy) = (x + dx, y + aim*dx, aim + dy)

main = do
  content <- readFile "2.dat"
  let changes = map parse_command (lines content)
  let final_position = foldl tuple_add (0, 0) changes
  let res1 = fst final_position * snd final_position
  print ("task 1: " ++ (show res1))

  let (x, y, aim) = foldl apply_command (0, 0, 0) changes
  let res2 = x * y
  print ("task 2: " ++ (show res2))
