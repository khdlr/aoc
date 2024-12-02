import Data.Char
import Data.List.Split
import Data.Set (Set)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Set as Set

data Voxel = Lava | Outside | Unknown deriving (Eq, Show)

data Tensor a = Tensor
  {storage :: Vector a, width :: Int, height :: Int, depth :: Int}
  deriving (Eq)

empty :: a -> Int -> Int -> Int -> Tensor a
empty el w h d =
  Tensor (V.replicate (w * h * d) el) w h d

(!) :: Tensor a -> (Int, Int, Int) -> a
(!) tensor pt = storage tensor V.! index tensor pt

index :: Tensor a -> (Int, Int, Int) -> Int
index tensor (x, y, z) = z + depth tensor * (y + height tensor * x)

unindex :: Tensor a -> Int -> (Int, Int, Int)
unindex tensor i = (x, y, z)
  where
    (yx, z) = i `divMod` depth tensor
    (x, y) = yx `divMod` height tensor

imap :: ((Int, Int, Int) -> a -> b) -> Tensor a -> Tensor b
imap fn tensor = Tensor (V.imap (fn . unindex tensor) storage) w h d
  where (Tensor storage w h d) = tensor

at :: Tensor Voxel -> (Int, Int, Int) -> Voxel
at tensor (x, y, z)
  | x < 0 || x >= w = Outside
  | y < 0 || y >= h = Outside
  | z < 0 || z >= d = Outside
  | otherwise = storage V.! index tensor (x, y, z)
  where (Tensor storage w h d) = tensor

insertLava :: Tensor Voxel -> [(Int, Int, Int)] -> Tensor Voxel
insertLava base coords = Tensor (storage V.// zip innerIdx (repeat Lava)) w h d
  where
    Tensor storage w h d = base
    innerIdx = map (index base) coords

list3ToTuple :: [Int] -> (Int, Int, Int)
list3ToTuple [a, b, c] = (a, b, c)

loadDroplet = do
  content <- readFile "18.dat"

  let
    out = map (list3ToTuple . (map read . splitOn ",")) $ lines content
  return out

neighbors = [
  (1, 0, 0), (-1, 0, 0),
  (0, 1, 0), (0, -1, 0),
  (0, 0, 1), (0, 0, -1)]

plus :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
(a, b, c) `plus` (x, y, z) = (a+x, b+y, c+z)

floodFillStep :: Tensor Voxel -> Tensor Voxel
floodFillStep tensor = imap update tensor
  where
    update :: (Int, Int, Int) -> Voxel -> Voxel
    update _ Lava = Lava
    update _ Outside = Outside
    update pt Unknown = if
      any ((==Outside) . (\n -> tensor `at` (pt `plus` n))) neighbors
        then Outside else Unknown

fixpointIteration :: Eq a => (a -> a) -> a -> a
fixpointIteration fn x
  | next == x = x
  | otherwise = fixpointIteration fn next
  where
    next = fn x

main = do
  droplet <- loadDroplet

  let
    dropletSet = Set.fromList droplet

    countFaces :: (Int, Int, Int) -> Int
    countFaces pt = length . filter ((`Set.notMember` dropletSet) . plus pt) $ neighbors

    totalFaces = sum . map countFaces $ droplet
    width  = 1 + (maximum . map (\(x,y,z) -> x) $ droplet)
    height = 1 + (maximum . map (\(x,y,z) -> y) $ droplet)
    depth  = 1 + (maximum . map (\(x,y,z) -> z) $ droplet)

    withoutLava = empty Unknown width height depth
    withLava = insertLava withoutLava droplet 

    floodFilled = fixpointIteration floodFillStep withLava

    countFaces2 pt = length . filter ((==Outside) . (floodFilled `at`) . plus pt) $ neighbors
    totalFaces2 = sum . map countFaces2 $ droplet

  putStrLn "Task 1"
  print totalFaces
  -- print . length . filter (==Outside) . V.toList . storage $ withLava
  -- print . length . filter (==Outside) . V.toList . storage . floodFillStep $ withLava
  -- print . length . filter (==Outside) . V.toList . storage $ floodFilled
  putStrLn "Task 2"
  print totalFaces2



