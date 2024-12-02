import Utils
import qualified Data.Set as Set
import Debug.Trace

data Axis = X | Y deriving (Read, Show, Eq, Enum, Bounded)
type Point = (Int, Int)
type Fold = (Axis, Int)

parseFold :: String -> Fold
parseFold row = (axis, loc)
  where
    axis = case head eqn of
      'x' -> X
      'y' -> Y
    loc  = parseInt $ drop 2 eqn
    eqn  = drop 11 row

parsePoint :: String -> Point
parsePoint row = 
  case nums of
    [a, b] -> (a, b)
  where
    nums = map parseInt . splitStr ',' $ row

applyFold :: Fold -> Point -> Point
applyFold (X, loc) (x, y)
  | x > loc   = (2 * loc - x, y)
  | otherwise = (x, y)
applyFold (Y, loc) (x, y)
  | y > loc   = (x, 2 * loc - y)
  | otherwise = (x, y)

doFold :: [Point] -> Fold -> [Point]
doFold points fold =
  map (applyFold fold) points

markChar :: [Point] -> Int -> Int -> Char
markChar points y x
  | (x, y) `elem` points  = '█'
  | otherwise             = ' '

main = do
  rows <- readLines "13.dat"
  let points = map parsePoint . takeWhile (/="") $ rows
  let folds  = map parseFold . tail . dropWhile (/="") $ rows
  -- print points
  let res1 = doFold points (head folds)
  print $ head folds
  print . length . Set.fromList $ res1

  let res2 = foldl doFold points folds

  let maxX = maximum . map fst $ res2
  let maxY = maximum . map snd $ res2

  let printrows = map (\y -> map (markChar res2 y) [0..maxX+1]) [0..maxY+1]
  mapM_ putStrLn printrows
