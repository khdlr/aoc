import Utils
import Data.MemoCombinators
import Data.HashMap (Map)
import qualified Data.HashMap as Map

costAt :: [[Int]] -> Int -> Int -> Int
costAt costs = memo2 integral integral inner
  where
    costTo = costAt costs
    inner :: Int -> Int -> Int
    inner 0 0 = 0
    inner 0 y = costTo 0 (y-1) + (costs !! 0 !! y)
    inner x 0 = costTo (x-1) 0 + (costs !! x !! 0)
    inner x y = (costs !! x !! y) + min (costTo x (y-1)) (costTo (x-1) y)


solvePuzzle :: [[Int]] -> Int
solvePuzzle costs =
  costAt costs maxX maxY
  where
    maxX = length costs - 1
    maxY = length (head costs) - 1

inflateRow :: [Int] -> [Int]
inflateRow row = [0..4] >>= (\x -> map (+x) row)

inflateMatrix :: [[Int]] -> [[Int]]
inflateMatrix mat =
  map (map wrap) rawMat
  where
    wrap x
      | x > 9     = wrap (x - 9)
      | otherwise = x
    rawMat = [0..4] >>= (\x -> map (map (+x) . inflateRow) mat)

type Point          = (Int, Int)
type NodeData       = ([Point], Int)
type DijkstraSet    = Map Point NodeData
type DijkstraState  = (DijkstraSet, DijkstraSet)
--newtype NodeData = NodeData ([Point], Int)
--instance Ord NodeData where
--  compare (NodeData (_, a)) (NodeData (_, b)) = compare a b

selectSmaller :: (a, (b, Int)) -> (a, (b, Int)) -> (a, (b, Int))
selectSmaller left@(_, (_, l)) right@(_, (_, r))
  | l <= r     = left
  | otherwise  = right


dijkstra :: [[Int]] -> Point -> Point -> NodeData
dijkstra costs a b =
  Map.findWithDefault (error "Graph not connected") b spCovering
  -- spCovering
  where
    maxX = length costs - 1
    maxY = length (head costs) - 1

    cost :: Point -> Int
    cost (x,y) = costs !! x !! y

    infinity = 1 + sum (map sum costs)
    initQ  = Map.fromList [ ((x, y), ([], infinity))
              | x <- [0..maxX], y <- [0..maxY] ]
    initQ' = Map.insert a ([], 0) initQ

    spCovering = dijkstraRecursion (Map.empty, initQ')

    updateNode :: (Point, NodeData) -> Point -> NodeData -> NodeData
    updateNode (ckpt, (ckpath, ckcost)) pt (oldpath, oldcost)
      | newcost < oldcost = (ckpt : ckpath, newcost)
      | otherwise         = (oldpath, oldcost)
      where newcost = ckcost + cost pt

    dijkstraRecursion :: DijkstraState -> DijkstraSet
    dijkstraRecursion (res, q)
      | Map.null q  = res
      | otherwise   = dijkstraRecursion (res', q'')
      where
        q'' = foldr (Map.adjustWithKey (updateNode minNode)) q' (neighbors minPoint)
        q'  = Map.delete minPoint q
        res' = Map.insert minPoint minData res
        (minPoint, minData) = minNode
        minNode = foldl1 selectSmaller (Map.assocs q)

    neighbors :: Point -> [Point]
    neighbors (x, y) = filter inside candidates
      where
        candidates = [(x-1, y), (x, y-1), (x+1, y), (x, y+1)]
        inside (x', y') = (x'>=0) && (y'>=0) && (x'<=maxX) && (y'<=maxY)

solvePuzzleDijkstra :: [[Int]] -> Int
solvePuzzleDijkstra costs = cost
  where
    (path, cost) = dijkstra costs (0, 0) (maxX, maxY)
    maxX = length costs - 1
    maxY = length (head costs) - 1


main = do
  rows <- readLines "15.dat"
  let costs = map (map (parseInt . (:[]))) rows

  print "Task 1:"
  print $ solvePuzzle costs
  print $ solvePuzzleDijkstra costs

  -- mapM_ (putStrLn . concatMap show) costs
  let bigCosts = inflateMatrix costs

  print "Task 2:"
  print $ solvePuzzle bigCosts
  print $ solvePuzzleDijkstra bigCosts
