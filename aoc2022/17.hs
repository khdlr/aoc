import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Vector (Vector, (!))
import Data.Matrix (Matrix, nrows, ncols, setElem, matrix, toLists, extendTo, (<->))
import Debug.Trace
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)

data Square = E | R deriving (Eq)
instance Show Square where
  show E = "·"
  show R = "#"

newtype Chamber = Chamber [Vector Square]
instance Show Chamber where
  show (Chamber rows) = "=======\n" ++ unlines (map (concatMap show) rows) ++ "=======\n"


data Piece = Piece
  { coords :: [(Int, Int)], width :: Int, height :: Int }

pieces = V.fromList [
  Piece [(0, 0), (0, 1), (0, 2), (0, 3)] 4 1,
  Piece [(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)] 3 3,
  Piece [(0, 2), (1, 2), (2, 0), (2, 1), (2, 2)] 3 3,
  Piece [(0, 0), (1, 0), (2, 0), (3, 0)] 1 4,
  Piece [(0, 0), (0, 1), (1, 0), (1, 1)] 2 2]

loadWind = do
  content <- readFile "17.dat"
  return $ V.fromList . map readChr . head . lines $ content
  where
    readChr '<' = -1
    readChr '>' = 1

canBlit :: Piece -> (Int, Int) -> Chamber -> Bool
canBlit piece (y, x) (Chamber rows)
  | (x < 0) || (x + width piece > 7) || 
    (y < 0) || (y + height piece > length rows) = False
  | otherwise = all checkSingle (coords piece)
  where
    checkSingle :: (Int, Int) -> Bool
    checkSingle (py, px) = (rows !! (y+py)) ! (x+px) == E

blit :: Piece -> (Int, Int) -> Chamber -> Chamber
blit piece (y, x) (Chamber rows) = Chamber $ zipWith blitRow [0..] rows
  where
    blitRow :: Int -> Vector Square -> Vector Square
    blitRow i row
      | i < (4+y) = V.fromList $ zipWith set [0..] (V.toList row)
      | otherwise = row
      where set j el = if (i-y, j-x) `elem` coords piece then R else el

traceTag :: Show a => String -> a -> a
traceTag tag el = trace (tag ++ ":\n" ++ show el) el

dropPiece :: Chamber -> (Int, Int) -> Vector Int -> (Chamber, (Int, Int))
dropPiece (Chamber rows) (iWind, iPiece) wind = (finalGrid, (iWindNext, (iPiece + 1) `mod` V.length pieces))
  where
    piece = pieces ! iPiece
    finalGrid = blit piece finalPos extended
    (finalPos, iWindNext) = dropStep (dropy, 2) iWind
    freeRows = length . takeWhile id . map (all (==E)) $ rows
    dropy = max (-extensionRows) 0
    extensionRows = height piece - freeRows + 3
    extended = Chamber $ replicate extensionRows (V.replicate 7 E) ++ rows
    dropStep :: (Int, Int) -> Int -> ((Int, Int), Int)
    dropStep (py, px) iWindInner
      | didStop = (afterFall, iWindInner+1)
      | otherwise = dropStep afterFall ((iWindInner+1) `mod` V.length wind)
      where
        currentWind = wind ! iWindInner
        afterWind = if canBlit piece (py, px+currentWind) extended
          then (py, px+currentWind) else (py, px)
        (ay, ax) = afterWind
        (didStop, afterFall) = if canBlit piece (ay+1, ax) extended
          then (False, (ay+1, ax)) else (True, (ay, ax))

towerHeight :: Chamber -> Int
towerHeight (Chamber rows) = length rows - freeRows
  where freeRows = length . takeWhile id . map (all (==E)) $ rows


main = do
  wind <- loadWind

  let
    windIdx = 0
    chamber = Chamber []

    iter :: (Chamber, (Int, Int)) -> (Chamber, (Int, Int))
    iter (chamber, (iWind, iPiece)) = dropPiece chamber (iWind, iPiece) wind

    scanSeq = iterate iter (chamber, (0, 0))
    chamberSeq = map fst scanSeq
    task1 = chamberSeq !! 2022

    -- untilRepeat = takeWhile ((/=(0, 0)) . snd) (tail scanSeq)
    withSet = scanl (flip (S.insert . snd)) S.empty scanSeq 

    updateHM :: HashMap (Int, Int) [Int] -> (Int, (Int, Int)) -> HashMap (Int, Int) [Int]
    updateHM hashmap (iteration, pair) = HM.insert pair newval hashmap
      where newval = iteration : HM.findWithDefault [] pair hashmap

  let
    cycleDetector = scanl updateHM HM.empty (zip [0..] (map snd scanSeq))
    withSet = scanl (flip (S.insert . snd)) S.empty scanSeq 

  mapM_ (\i -> do {print i; print (chamberSeq !! i)}) [1..5]

  let
    cycleIndicators = HM.filter ((>2) . length) (cycleDetector !! 4000)

    diff1 :: [Int] -> [Int]
    diff1 [] = []
    diff1 [x] = []
    diff1 (x1:x2:xs) = x1-x2:diff1 (x2:xs)

    allequal :: Eq a => [a] -> Maybe a
    allequal [] = Nothing
    allequal [x] = Just x
    allequal (x1:x2:xs)
      | x1 == x2 = allequal (x2:xs)
      | otherwise = Nothing

    trueCycles = mapMaybe (allequal . diff1) $ HM.elems cycleIndicators
    Just cycleLength = allequal trueCycles
    someCycle = head $ filter
      (\x -> case allequal (diff1 x) of Nothing -> False; Just _ -> True)
      (HM.elems cycleIndicators)

    Just heightPerCycle =
      allequal . diff1 . map (towerHeight . (chamberSeq!!)) $ someCycle

    hingePoint = head someCycle
    (numCycles, remaining) = (1000000000000 - hingePoint) `divMod` cycleLength
    finalHeight = numCycles * heightPerCycle +
      towerHeight (chamberSeq!! (hingePoint + remaining))

  putStrLn "Task 1"
  print . towerHeight $ task1
  putStrLn "Task 2"
  print finalHeight
  putStrLn "Done"
