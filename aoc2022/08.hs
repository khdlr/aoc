import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Matrix as M
import Data.Matrix (Matrix, (!))

readchr :: Char -> Int
readchr x = read [x]

visScanner :: (Bool, Int) -> Int -> (Bool, Int)
visScanner (visible, peakHeight) current 
  | current > peakHeight = (True, current)
  | otherwise = (False, peakHeight)

mapRows :: (Vector a -> Vector b) -> Matrix a  -> Matrix b
mapRows fun = M.fromRows . map fun . M.toRows

mapCols :: (Vector a -> Vector b) -> Matrix a  -> Matrix b
mapCols fun = M.fromColumns . map fun . M.toColumns

treeVisible :: Matrix Int -> Matrix Bool
treeVisible heights = M.zipWith (||)
    (M.zipWith (||) (mapRows leftScan heights) (mapRows rightScan heights))
    (M.zipWith (||) (mapCols leftScan heights) (mapCols rightScan heights))
  where
    leftScan = V.map fst . V.postscanl visScanner (False, -1)
    rightScan = V.map fst . V.postscanr (flip visScanner) (False, -1)


viewDistance :: Matrix Int -> Int -> (Int, Int) -> (Int, Int) -> Int
viewDistance grid treeHeight (y, x) (dy, dx)
  | y < 0 || x < 0 || y >= M.rows grid || x >= M.cols grid = 0
  | grid ! (y, x) >= treeHeight = 1
  | otherwise = 1 + viewDistance grid treeHeight (y+dy, x+dx) (dy, dx)

scenicScore :: Matrix Int -> (Int, Int) -> Int
scenicScore grid (y, x) = product dirScores
  where
    dirScores = map
      (\(dy, dx) -> viewDistance grid treeHeight (y+dy, x+dx) (dy, dx)) directions
    directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    treeHeight = grid ! (y, x)


main = do
  content <- readFile "08.dat"
  let
    grid = M.fromLists . map (map readchr) $ lines content
    visible = treeVisible grid
    points = [(y, x) | y <- [1..M.rows grid-2], x <- [1..M.cols grid-2]]

  print "Task 1"
  print . show . length . filter id . M.toList $ visible

  print "Task 2"
  print . show . maximum . map (scenicScore grid) $ points

