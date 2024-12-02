import Data.List
import Text.Regex.PCRE
import Text.Regex.PCRE.String ()

type IntervalSet = [(Int, Int)]
nothing = [] :: IntervalSet
add :: IntervalSet -> Maybe (Int, Int) -> IntervalSet
add intervals Nothing = intervals
add intervals (Just (start, end)) = (startNew, endNew):keep
  where
    (merge, keep) = partition touched intervals
    touched :: (Int, Int) -> Bool
    touched (a, b) = (a <= end+1) && (b >= start-1)
    startNew :: Int
    startNew = minimum $ start:map fst merge
    endNew = maximum $ end:map snd merge

bound :: (Int, Int) -> IntervalSet -> IntervalSet
bound (start, end) = map crop . filter inside
  where
    crop (a, b) = (max start a, min end b)
    inside (a, b) = a <= end || b >= start

count :: IntervalSet -> Int
count = foldl (\c (a, b) -> b - a + 1 + c) 0

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine line = ((sx, sy), (bx, by))
  where
    [[_, sx, sy, bx, by]] = map (map read) (line =~ lineRE)
    lineRE = "Sensor at x=(\\-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)"

dist :: ((Int, Int), (Int, Int)) -> Int
dist ((sx, sy), (bx, by)) = abs (sx - bx) + abs (sy - by)

blockedInterval :: Int -> ((Int, Int), (Int, Int)) -> Maybe (Int, Int)
blockedInterval row measurement
  | drow <= d = Just (sx - d + drow, sx + d - drow)
  | otherwise = Nothing
  where
    ((sx, sy), _) = measurement
    d = dist measurement
    drow = abs (sy - row)

loadData = do
  content <- readFile "15.dat"
  return . map parseLine . lines $ content

blocked :: [((Int, Int), (Int, Int))] -> Int -> Int
blocked beacons row = 0

withIndex :: [a] -> [(Int, a)]
withIndex = zip [0..]

main = do
  sensors <- loadData
  let
    row = 2000000
    cover = foldl add nothing $ map (blockedInterval row) sensors
    covered = count cover
    present = length . nub . filter ((==row) . snd) . map snd $ sensors
    result = covered - present
  putStrLn "Task 1"
  print (covered, present, result)

  let
    rowcover r = foldl add nothing $ map (blockedInterval r) sensors
    rowcovers = map (bound (0, 2*row) . rowcover) [0..2*row] 
    -- [(tRow, intervals)] 
    filtered = filter ((>1) . length . snd) . withIndex $ rowcovers 
    -- 
    results = map (map ((+1) . snd) . filter ((==0) . fst) . snd) filtered
    out = concatMap (\((r, _), cs) -> map ((r+) . (4000000*)) cs) $ zip filtered results
    

  putStrLn "Task 2"
  -- print (tCol * 4000000 + tRow)
  print out
