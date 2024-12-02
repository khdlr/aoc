import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.Matrix (Matrix, (!))
import qualified Data.Matrix as Matrix
import qualified Data.PQueue.Prio.Min as Q
import Debug.Trace

data Blizzard = Blizzard Int Int Int Int

parseChar :: (Int, Int) -> Char -> [Blizzard]
parseChar (y, x) 'v' = [Blizzard y x 1 0]
parseChar (y, x) '>' = [Blizzard y x 0 1]
parseChar (y, x) '<' = [Blizzard y x 0 (-1)]
parseChar (y, x) '^' = [Blizzard y x (-1) 0]
parseChar _ _ = []

loadData :: IO (Matrix Bool, [Blizzard], (Int, Int), (Int, Int))
loadData = do
  rows <- lines <$> readFile "24.dat"
  let
    baseMap = fmap (/='#') . Matrix.fromLists $ rows
    field = concat . Matrix.toList . Matrix.mapPos parseChar . Matrix.fromLists $ rows
    Just startx = fmap (+1) ('.' `elemIndex` head rows)
    Just goalx = fmap (+1) ('.' `elemIndex` last rows)
    height = length rows
    width = (length . head) rows
  return (baseMap, field, (1, startx), (length rows, goalx))

stepBlizzard :: (Int, Int) -> Blizzard -> Blizzard
stepBlizzard (height, width) (Blizzard y x dy dx) = Blizzard ny nx dy dx
  where
    ny
      | y == 2 && dy == -1 = height-1
      | y == height-1 && dy == 1 = 2
      | otherwise = y + dy
    nx
      | x == 2 && dx == -1 = width-1
      | x == width-1 && dx == 1 = 2
      | otherwise = x + dx

drawBlizzards :: Matrix Bool -> [Blizzard] -> Matrix Bool
drawBlizzards baseMap [] = baseMap
drawBlizzards baseMap ((Blizzard y x _ _):xs) = Matrix.setElem False (y, x) $ drawBlizzards baseMap xs

showBool False = '#'
showBool True = ' '

printField :: Matrix Bool -> IO ()
printField field = putStrLn $ "=======\n" ++
    unlines (map (map showBool) (Matrix.toLists field)) ++
    "=======\n"

free :: [Matrix Bool] -> (Int, Int, Int) -> Bool
free fields (y, x, t) = fromMaybe False (Matrix.safeGet y x (fields !! t))

distBFS:: [Matrix Bool] -> (Int, Int, Int) -> (Int, Int) -> Int
distBFS fields st (ty, tx) = bfs (Q.singleton (prio st) st) (Set.singleton st)
  where
    nextStates :: (Int, Int, Int) -> [(Int, Int, Int)]
    nextStates (y, x, t) = filter (free fields) . map (\(a,b) -> (a,b,t+1)) $ moves
      where
        moves = [(y, x), (y-1, x), (y, x-1), (y+1, x), (y, x+1)]

    prio :: (Int, Int, Int) -> Int
    prio (y, x, t) = t + abs (y - ty) + abs (x - tx)

    bfs :: Q.MinPQueue Int (Int, Int, Int) -> Set (Int, Int, Int) -> Int
    bfs inputQ inputSet
      | Q.null inputQ      = -1
      | (y, x) == (ty, tx) = t
      | otherwise          = bfs insertedQ insertedSet
      where
        (_, (y, x, t)) = traceShowId (Q.findMin inputQ)
        poppedQ = Q.deleteMin inputQ
        next = filter (`Set.notMember` inputSet) $ nextStates (y, x, t)
        insertedQ = foldr (\st -> Q.insert (prio st) st) poppedQ next
        insertedSet = foldr Set.insert inputSet next


main :: IO ()
main = do
  (baseMap, blizzards, (sy, sx), (ty, tx)) <- loadData

  let
    height = Matrix.nrows baseMap
    width = Matrix.ncols baseMap

    blizzardPos = (iterate . map . stepBlizzard $ (height, width)) blizzards
    blizzardMap = map ((drawBlizzards baseMap) $!) blizzardPos

    dash1 = distBFS blizzardMap (sy, sx, 0) (ty, tx)
    dash2 = distBFS blizzardMap (ty, tx, dash1) (sy, sx)
    dash3 = distBFS blizzardMap (sy, sx, dash2) (ty, tx)

  print dash1
  print dash2
  print dash3
