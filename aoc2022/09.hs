import Data.List

applyCommand :: String -> [[(Int, Int)]] -> [[(Int, Int)]]
applyCommand cmd input = take count . tail $ iterate (applyStep direction) (last input)
  where
    [dir, count_raw] = words cmd
    count = read count_raw
    direction = case dir of
      "U" -> ( 0,  1)
      "D" -> ( 0, -1)
      "L" -> (-1,  0)
      "R" -> ( 1,  0)

applyStep :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
applyStep (dy, dx) ((hy, hx):rest) = updateSecond ((hy', hx'):rest)
  where
    hy' = hy + dy
    hx' = hx + dx

updateSecond :: [(Int, Int)] -> [(Int, Int)]
updateSecond [] = []
updateSecond [single] = [single]
updateSecond ((hy', hx'):(ty, tx):rest) =
  (hy', hx'):updateSecond ((ty', tx'):rest)
  where
    stepNeeded = abs (ty - hy') >= 2 ||
                 abs (tx - hx') >= 2 || 
                 abs (ty - hy') + abs (tx - hx') >= 3
    diagonalStep = stepNeeded && (ty /= hy') && (tx /= hx')
    ty' | (ty == hy') || not stepNeeded = ty
        | ty < hy' = ty + 1
        | ty > hy' = ty - 1
    tx' | (tx == hx') || not stepNeeded = tx
        | tx < hx' = tx + 1
        | tx > hx' = tx - 1

makeChain n = replicate n (0, 0)

main = do
  content <- readFile "09.dat"
  let
    commands = lines content
    trace2  = concat $ scanl (flip applyCommand) [replicate 2 (0, 0)] commands
    trace10 = concat $ scanl (flip applyCommand) [replicate 10 (0, 0)] commands
    tailPos2 = map last trace2
    tailPos10 = map last trace10

  print "Task 1"
  print . show . length . nub $ tailPos2

  print "Task 2"
  -- mapM (print . show) trace10
  print . show . length . nub $ tailPos10

