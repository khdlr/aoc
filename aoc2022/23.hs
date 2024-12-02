import Data.List
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Matrix (Matrix, (!), (<|>), (<->))
import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Matrix as Matrix
import Debug.Trace

readChr :: Char -> Bool
readChr '#' = True
readChr '.' = False

showChr :: Bool -> Char
showChr True = '#'
showChr False = '.'

type Field = Matrix Bool

printField :: Field -> IO ()
printField field = putStrLn $ "=======\n" ++
    unlines (map (map showChr) (Matrix.toLists field)) ++
    "=======\n"

showField field = "=======\n" ++
    unlines (map (map showChr) (Matrix.toLists field)) ++
    "=======\n"

loadData :: IO Field
loadData = do
  raw <- readFile "23.dat"
  let
    field = Matrix.fromLists . map (map readChr) . lines $ raw
  return field

get :: Field -> (Int, Int) -> Bool
get mat (i, j) = fromMaybe False (Matrix.safeGet i j mat)

empty :: Field -> (Int, Int) -> Bool
empty f m = not $ get f m 

elfProposal :: Field -> Int -> (Int, Int) -> [(Int, Int)]
elfProposal field iteration (i, j)
  | noNeighbors   = []
  | checkMode 0   = moveTo 0
  | checkMode 1   = moveTo 1
  | checkMode 2   = moveTo 2
  | checkMode 3   = moveTo 3
  | otherwise     = []
  where
    noNeighbors = all (empty field) . nub . concat $ modes
    checkMode :: Int -> Bool
    checkMode i = all (empty field) (modes !! ((i + iteration) `mod` 4))
    moveTo :: Int -> [(Int, Int)]
    moveTo = singleton . head . (modes!!) . (`mod`4) . (+iteration)
    modes = [
      [(i-1, j), (i-1, j-1), (i-1, j+1)],  -- (N)
      [(i+1, j), (i+1, j-1), (i+1, j+1)],  -- (S)
      [(i, j-1), (i+1, j-1), (i-1, j-1)],  -- (E)
      [(i, j+1), (i+1, j+1), (i-1, j+1)]   -- (W)
      ]

proposals :: Field -> Int -> [(Int, Int)]
proposals field i = concat . Matrix.toList . Matrix.mapPos prop $ field
  where
    prop :: (Int, Int) -> Bool -> [(Int, Int)]
    prop _ False = []
    prop pt True = elfProposal field i pt

counts :: (Hashable a, Eq a) => [a] -> HashMap a Int
counts = foldr (\x -> HashMap.insertWith (+) x 1) HashMap.empty

zeros h w = Matrix.matrix h w (const False)

argwhere :: Matrix Bool -> [(Int, Int)]
argwhere = map fst . filter snd . Matrix.toList . Matrix.mapPos (,)

step :: (Field, Int) -> (Field, Int)
step (field, i) = (cropped, i+1)
  where
    vbar = zeros (Matrix.nrows field) 1
    hbar = zeros 1 (2 + Matrix.ncols field)
    extended = hbar <-> (vbar <|> field <|> vbar) <-> hbar
    props = proposals extended i
    accepted = extractAccepted . HashMap.toList . counts $ props
    extractAccepted = map fst . filter ((==1) . snd)
    stepped = Matrix.mapPos updateSquare extended 

    indices = argwhere stepped
    miny = minimum . map fst $ indices
    maxy = maximum . map fst $ indices
    minx = minimum . map snd $ indices
    maxx = maximum . map snd $ indices
    cropped = Matrix.submatrix miny maxy minx maxx stepped

    updateSquare :: (Int, Int) -> Bool -> Bool
    updateSquare pt filled
      | pt `elem` accepted = True
      | filled = case elfProposal extended i pt of
        [prop] -> prop `notElem` accepted
        [] -> True
      | otherwise = False

score :: Field -> Int
score = length . filter (==False) . Matrix.toList

fixpoint :: Eq a => ((a, b) -> (a, b)) -> (a, b) -> (a, b)
fixpoint fn x
  | fst next == fst x = x
  | otherwise = fixpoint fn next
  where
    next = fn x

traceField :: (Field, Int) -> (Field, Int)
traceField (f, i) = trace (showField f ++ show i) (f, i)

traceSnd :: (Field, Int) -> (Field, Int)
traceSnd (f, i) = traceShow i (f, i)

main = do
  field <- loadData
  let
    fieldSeq = map fst . iterate step $ (field, 0)
  -- printField . (!!10) $ fieldSeq
  print . score . (!!10) $ fieldSeq
  print . (+1) . snd . fixpoint (traceField . step) $ (field, 0)
