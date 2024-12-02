import Debug.Trace
import Data.HashMap hiding (map, filter)
import Text.RegexPR

type Point = (Int, Int)
type PointCounter = Map Point Int

readline :: [Char] -> (Point, Point)
readline line =
  case ints of
    [y1, x1, y2, x2] -> ((y1, x1), (y2, x2))
  where
    captures = getbrsRegexPR "(\\d+),(\\d+) -> (\\d+),(\\d+)" line
    ints = map (\x -> read x :: Int) (tail captures)

mkrange :: Int -> Int -> [Int]
mkrange a b
  | a < b = [a..b]
  | a > b = [a,a-1..b]

linePoints :: (Point, Point) -> [Point]
linePoints ((y1, x1), (y2, x2))
  | y1 == y2 && x1 /= x2 = map (\x -> (y1, x)) (mkrange x1 x2)
  | y1 /= y2 && x1 == x2 = map (\y -> (y, x1)) (mkrange y1 y2)
  | y1 /= y2 && x1 /= x2 = zip (mkrange y1 y2) (mkrange x1 x2)

isntDiagonal :: (Point, Point) -> Bool
isntDiagonal ((y1, x1), (y2, x2)) = (y1 == y2) || (x1 == x2)

countElementsStep :: Point -> PointCounter -> PointCounter
countElementsStep el counter =
  insert el (currentCount+1) counter
  where currentCount = findWithDefault 0 el counter 

countOverlaps filterPredicate segments =
  length (filter (\(k, v) -> v > 1) (assocs counts))
  where
    filtered = filter filterPredicate segments
    points = filtered >>= linePoints
    counts = foldr countElementsStep (empty :: Map Point Int) points


main = do
  content <- readFile "5.dat"
  let segments = map readline (lines content)

  -- Task 1
  print . show $ countOverlaps isntDiagonal segments
  
  -- Task 2
  print . show $ countOverlaps (\x -> True) segments
