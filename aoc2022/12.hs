import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Matrix as M
import Data.Matrix (Matrix, (!))
import Data.Char (ord)
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Debug.Trace (trace)
import Queue

type Point = (Int, Int) 

readHeight :: Char -> Int
readHeight 'S' = readHeight 'a'
readHeight 'E' = readHeight 'z'
readHeight c = ord c - ord 'a'


loadField :: IO (Matrix Int, Point, Point, [Point])
loadField = do
  content <- readFile "12.dat"
  let
    chrs = M.fromLists . lines $ content
    field = fmap readHeight chrs
    coordsAndChr = M.toList . M.mapPos (,) $ chrs
    start = fst . head . filter ((=='S') . snd) $ coordsAndChr
    goal = fst . head . filter ((=='E') . snd) $ coordsAndChr
    possibleStarts = map fst . filter ((\x -> x=='a' || x =='S') . snd) $ coordsAndChr
  return (field, start, goal, possibleStarts)

dist :: Matrix Int -> Point -> Point -> Int
dist heights start end = bfs (Q (Seq.singleton (start, 0))) (S.singleton start)
  where
    bfs :: Queue (Point, Int) -> S.Set Point -> Int
    bfs inputQueue visited
      | isempty inputQueue = 999999999999
      | current == end = currentDist
      | otherwise = bfs queueWithNeighbors visitedWithNeighbors
      where
        ((current, currentDist), poppedQueue) = dequeue inputQueue
        queueWithNeighbors = enqueueMulti poppedQueue $ map (\x -> (x,currentDist+1)) . filter (not . flip S.member visited) $ neighbors current
        visitedWithNeighbors = foldl (flip S.insert) visited $ neighbors current
    neighbors :: Point -> [Point]
    neighbors (y, x) = filter validNeighbor [(y+1, x), (y-1, x), (y, x+1), (y, x-1)]
      where 
        validNeighbor (ny, nx) = nx > 0 && ny > 0
          && nx <= M.ncols heights && ny <= M.nrows heights
          && heights ! (y, x) >= heights ! (ny, nx) - 1

main = do
  (field, start, goal, possibleStarts) <- loadField

  putStrLn "Task 1"
  print $ dist field start goal

  putStrLn "Task 2"
  print . minimum . map (\s -> dist field s goal) $ possibleStarts

