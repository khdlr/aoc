import Text.Regex.PCRE
import Text.Regex.PCRE.String ()
import GHC.Data.Maybe (orElse)
import Data.HashMap.Strict (HashMap, fromList, toList, (!?), findWithDefault, filterWithKey)
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.List.Split
import Debug.Trace
import Queue

data Chamber = Chamber
  { flow :: Int, neighbors :: [String] }
  deriving (Eq, Show)

line = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"

parseLine :: String -> (String, Chamber)
parseLine line = (name, Chamber (read rate) neighbors)
  where
    [[_, name, rate, neighborsStr]] = line =~ lineRE
    lineRE = "Valve (.+) has flow rate=(\\d+); tunnel.+valve(?:s)? (.+)"
    neighbors = splitOn ", " neighborsStr

distances :: HashMap String Chamber -> HashMap String [(String, Int)]
distances chambers = fromList [(a, [(b, 1 + distBFS chambers a b) | b <- filter (/=a) nodes]) | a <- nodes]
  where
    interesting (name, Chamber f n) = name == "AA" || f > 0
    nodes = map fst . filter interesting $ toList chambers

distBFS:: HashMap String Chamber -> String -> String -> Int
distBFS chambers start end = bfs (singleton (start, 0)) (S.singleton start)
  where
    bfs :: Queue (String, Int) -> S.Set String -> Int
    bfs inputQueue visited
      | isempty inputQueue = 999999999999
      | current == end = currentDist
      | otherwise = bfs queueWithNeighbors visitedWithNeighbors
      where
        ((current, currentDist), poppedQueue) = dequeue inputQueue
        queueWithNeighbors = enqueueMulti poppedQueue $ map (\x -> (x, currentDist+1)) . filter (not . flip S.member visited) $ neig
        visitedWithNeighbors = foldl (flip S.insert) visited neig
        neig = fmap neighbors (chambers !? current) `orElse` []

search :: HashMap String Chamber -> Int -> [([String], Int)]
search chambers time = continue ["AA"] time 0
  where
    dist = distances chambers
    continue :: [String] -> Int -> Int -> [([String], Int)]
    continue path time score = (path, score) : concatMap buildNext nextValves
      where
        buildNext :: (String, Int) -> [([String], Int)]
        buildNext (name, d) = continue (name:path) (time - d) (score + (time - d) * f) 
          where f = fmap flow (chambers !? name) `orElse` 0
        current = head path
        notVisited = (`notElem` path) . fst
        stillPossible = (<=time) . snd
        nextValves = filter stillPossible . filter notVisited $ findWithDefault [] current dist

loadField = do
  content <- readFile "16.dat"
  return $ fromList (map parseLine . lines $ content)

main = do
  field <- loadField
  let
    out = search field 30
  putStrLn "Task 1"
  print . maximum . map snd $ out

  let
    myPaths = search field 26
    augment (visited, score) = map (\(p, s) -> ((visited, p), score + s)) elephantPaths
      where
        elephantPaths = search remainingMap 26
        remainingMap = filterWithKey (\k v -> k == "AA" || k `notElem` visited) field
    pairPaths = concatMap augment myPaths
  print . maximum . map snd $ pairPaths
  putStrLn "Done"
