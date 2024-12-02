{-# LANGUAGE TemplateHaskell #-}

import Data.Maybe (fromMaybe)
import Data.Char (isDigit)
import Data.List.Split
import Data.Matrix (Matrix, (!))
import qualified Data.Matrix as Matrix
import Control.Lens hiding (Empty)
import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as Set

data Dir = N | E | W | S deriving (Eq, Show)
rawLeft :: Dir -> Dir
rawLeft x = case x of
  N -> W
  W -> S
  S -> E
  E -> N

data Square = Empty | Wall | Magic deriving (Eq)
instance Show Square where
  show Empty = "·"
  show Wall  = "#"
  show Magic = " "

readChr :: Char -> Square
readChr ' ' = Magic
readChr '#' = Wall
readChr '.' = Empty

data State = State { _row :: Int, _col :: Int, _dir :: Dir } deriving (Eq, Show)
makeLenses ''State
pos :: State -> (Int, Int)
pos (State r c d) = (r, c)

left = over dir rawLeft
opposite = left . left
right = opposite . left

padWith :: a -> [[a]] -> [[a]]
padWith val rows = map pad rows
  where
    pad row
      | length row < maxLen = row ++ replicate (maxLen - length row) val 
      | otherwise = row
    maxLen = maximum . map length $ rows


newtype Cave = Cave (Matrix Square)
instance Show Cave where
  show (Cave mat) = "=======\n" ++ unlines (map (concatMap show)
    (Matrix.toLists mat)) ++ "=======\n"

loadData :: IO (Cave, String)
loadData = do
  raw <- readFile "22.dat"
  let
    [mapRaw, moves] = splitOn "\n\n" raw
    cave :: Cave
    cave = Cave . Matrix.fromLists . padWith Magic . map (map readChr) . lines $ mapRaw
  return (cave, moves)

get :: Cave -> (Int, Int) -> Square
get (Cave mat) (i, j) = fromMaybe Magic (Matrix.safeGet i j mat)

step :: Cave -> State -> State
step cave (State i j dir)
  | isMagicStep = magicTarget
  | otherwise   = State (i+di) (j+dj) dir
  where
    isMagicStep = cave `get` (i+di, j+dj) == Magic

    magicStep :: State -> State
    magicStep (State i' j' dir) = case cave `get` (ni, nj) of
      Magic -> State i' j' dir
      _ -> magicStep (State ni nj dir)
      where
        (ni, nj) = (i'-di, j'-dj)
    magicTarget = magicStep (State i j dir)
    (di, dj) = case dir of
      N -> (-1,  0)
      W -> ( 0, -1)
      S -> ( 1,  0)
      E -> ( 0,  1)

stepCube :: Cave -> State -> State
stepCube cave (State i j dir)
  | isMagicStep = magicTarget
  | otherwise   = State (i+di) (j+dj) dir
  where
    isMagicStep = cave `get` (i+di, j+dj) == Magic

    -- glue :: (Int, Int) -> (Int, Int) -> ???

    magicStep :: State -> State
    magicStep (State i' j' dir) = case ((i'-1) `div` 50, (j'-1) `div` 50, dir) of
      (0, 2, S) -> State (j' - 50) (i'+50) W
      (1, 1, E) -> State 50 (i'+50) N
      (2, 0, W) -> State (151 - i') (j'+50) E
      (0, 1, W) -> State (151 - i') (j'-50) E
      (2, 0, N) -> State (j' + 50) 51 E
      (1, 1, W) -> State 101 (i' - 50) S
      (2, 1, S) -> State (100 + j') 50 W
      (3, 0, E) -> State 150 (i' - 100) N
      (0, 1, N) -> State (j' + 100) 1 E
      (2, 1, E) -> State (151 - i') 150 W
      (0, 2, E) -> State (151 - i') 100 W
      (3, 0, S) -> State 1 (j' + 100) S
      (0, 2, N) -> State 200 (j' - 100) N
      (3, 0, W) -> State 1 (i' - 100) S
      a -> error ("Missing wrapping behaviour for " ++ show a)

    magicTarget = magicStep (State i j dir)
    (di, dj) = case dir of
      N -> (-1,  0)
      W -> ( 0, -1)
      S -> ( 1,  0)
      E -> ( 0,  1)

move :: (Cave -> State -> State) -> Cave -> Int -> State -> State
move stepfn cave 0 state = state
move stepfn cave amt state
  | steppedSquare == Wall  = state
  | steppedSquare == Empty = move stepfn cave (amt-1) stepped
  | otherwise              = error "Not Implemented"
  where
    steppedSquare = cave `get` pos stepped
    stepped = stepfn cave state

parseMove :: (Cave -> State -> State) -> Cave -> String -> State -> State
parseMove stepfn cave "L" = left
parseMove stepfn cave "R" = right
parseMove stepfn cave num = move stepfn cave (read num)

password :: State -> Int
password (State r c d) = 1000 * r + 4 * c + case d of
  E -> 0
  S -> 1
  W -> 2
  N -> 3

-- buildCube :: Matrix Bool -> [(Int, Int)]
-- buildCube tiles = edges `seq` facefns
--   where
--     faces = map fst . filter snd . Matrix.toList . Matrix.mapPos (,) $ tiles
--     d1 ((i, j), (i', j')) = abs (i - i') + abs (j - j')
--     pair2set (a, b) = Set.fromList [a, b]
--     edges = traceShowId . Set.fromList . map pair2set . filter ((==1)  . d1) $ (,) <$> faces <*> faces

-- second (x:y:z:xs) = x : second xs;
-- second _ = []
second = id
scl i = (i+1) `div` 1

plotCave :: Cave -> State -> IO ()
plotCave (Cave mat) (State i j dir) = putStrLn $ "=======\n" ++
  unlines (second (map (concat . second) $ Matrix.toLists (Matrix.mapPos showPix mat))) ++ "=======\n"
  where
    showPix (pi, pj) pt
      | (scl pi , scl pj) == (scl i, scl j) = case dir of
        N -> "^"
        S -> "v"
        E -> ">"
        W -> "<"
      | otherwise = show pt

main = do
  (cave, moveString) <- loadData

  let
    Cave mat = cave
    startx = length . takeWhile (==Magic) . head . Matrix.toLists $ mat
    state = State 1 (startx+1) E
    commands = split (oneOf "LR") moveString

    moves = map (parseMove step cave) commands
    sequence = scanl (flip ($)) state moves

    cubeMoves = map (parseMove stepCube cave) commands
    cubeSequence = scanl (flip ($)) state cubeMoves

  print . password . last $ sequence
  print (last cubeSequence)
  print . password . last $ cubeSequence
  -- mapM_ (\(x, y) -> getLine >> putStrLn ("Command: " ++ y) >> plotCave cave x) (zip cubeSequence commands)

