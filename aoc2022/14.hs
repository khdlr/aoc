{-# LANGUAGE TupleSections #-}

import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Matrix as M
import Data.Matrix (Matrix(..), (!))
import Data.Char (ord)
import Data.List.Split
import Debug.Trace

data Square = Empty | Wall | Sand deriving (Eq)
instance Show Square where
  show Empty = "░"
  show Wall  = "█"
  show Sand  = "⠿"

parsePair :: String -> (Int, Int)
parsePair pair = (y, x)
  where
    [x, y] = map read . splitOn "," $ pair

drawRow :: Matrix Square -> [(Int, Int)] -> Matrix Square
drawRow previous coords = foldl drawLine previous (zip coords (tail coords))

drawLine :: Matrix Square -> ((Int, Int), (Int, Int)) -> Matrix Square
drawLine previous ((y0, x0), (y1, x1))
  | y0 == y1 = foldl (flip (M.setElem Wall)) previous (map (y0,) ([x0..x1] ++ [x1..x0]))
  | x0 == x1 = foldl (flip (M.setElem Wall)) previous (map (,x0) ([y0..y1] ++ [y1..y0]))

loadField :: IO (Matrix Square, Int)
loadField = do
  content <- readFile "14.dat"
  let
    points = map (map parsePair . splitOn " -> ") $ lines content
    ys = concatMap (map fst) points
    xs = concatMap (map snd) points
    ymin = 0
    ymax = maximum ys
    xmin = minimum $ (500 - height):xs
    xmax = maximum $ (500 + height):xs

    height = ymax - ymin + 2
    width = xmax - xmin + 1

    adjustedPoints = map (map (\(y, x) -> (y+1, x-xmin+1))) points

    emptyField = M.matrix height width (const Empty)
    fullField = foldl drawRow emptyField adjustedPoints

  return (fullField, 500 - xmin + 1)

simulateSand :: (Int, Int) -> Matrix Square -> Matrix Square
simulateSand source square = nextSquare
  where
    nextSquare = case step source of
      Nothing -> square
      Just coords -> M.setElem Sand coords square
    step :: (Int, Int) -> Maybe (Int, Int)
    step (y, x)
        | y+1 > nrows square  = Nothing
        | square ! (y+1, x)   == Empty = step (y+1, x)
        | square ! (y+1, x-1) == Empty = step (y+1, x-1)
        | square ! (y+1, x+1) == Empty = step (y+1, x+1)
        | otherwise = Just (y, x)

fixpointIteration :: Eq a => (a -> a) -> a -> a
fixpointIteration fn x
  | next == x = x
  | otherwise = fixpointIteration fn next
  where
    next = fn x


main = do
  (field, sourcex) <- loadField

  putStrLn "Task 1"
  print (show (M.nrows field) ++ " x " ++ show (M.ncols field))

  let
    fixpoint = fixpointIteration (simulateSand (1, sourcex)) field
  -- putMatrix fixpoint
  print . length . filter (==Sand) . M.toList $ fixpoint

  putStrLn "Task 2"
  let
    grown = M.setSize Wall (M.nrows field + 3) (M.ncols field) field
    fixpoint2 = fixpointIteration (simulateSand (1, sourcex)) grown
  print (show (M.nrows grown) ++ " x " ++ show (M.ncols grown))
  -- putMatrix fixpoint2
  -- putMatrix $ iterate (simulateSand (0, sourcex)) grown !! 40
  print . length . filter (==Sand) . M.toList $ fixpoint2

putMatrix :: Show a => Matrix a -> IO ()
putMatrix m = putStrLn $ concat
   [ "┌ ", concat (replicate (ncols m) " "), " ┐\n"
   , unlines
   [ "│ " ++ concatMap (\j -> show (m ! (i,j))) [1..ncols m] ++ " │" | i <- [1..nrows m] ]
   , "└ ", concat (replicate (ncols m) " "), " ┘"
   ]

