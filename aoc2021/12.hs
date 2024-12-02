import Utils
import Data.Char
import Data.List
import Debug.Trace

type Edge = (String, String)

isBig :: String -> Bool
isBig = all isUpper

isSmall :: String -> Bool
isSmall = all isLower

readConnection :: String -> Edge
readConnection x =
  case splits of
    [a, b] -> (a, b)
    _      -> undefined
  where splits = splitStr '-' x

completePath :: ([String] -> String -> Bool) -> [Edge] -> [String] -> [[String]]
completePath _ _ [] = []
completePath canVisit edges path@(loc:rest) =
  map (:path) finished ++ (unfinished >>= (completePath canVisit edges . (:path)))
  where
    (finished, unfinished) = partition (=="end") choices
    choices    = filter (canVisit path) candidates
    candidates = map snd (filter ((==loc) . fst) edges) ++ 
                 map fst (filter ((==loc) . snd) edges)

canVisitT1 :: [String] -> String -> Bool
canVisitT1 path candidate = isBig candidate || (candidate `notElem` path)


allUnique :: Eq a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs && allUnique xs

noSmallCaveTwice :: [String] -> Bool
noSmallCaveTwice path =
  allUnique smallCaves
  where
    smallCaves = filter isSmall path 
    

canVisitT2 :: [String] -> String -> Bool
canVisitT2 path candidate =
  candidate /= "start" &&
    (isBig candidate || 
    candidate `notElem` path ||
    noSmallCaveTwice path
    )
  where
    timesVisited c = length . filter (==c) $ path

joinstr x y = x++","++y

main = do
  rows <- readLines "12.dat"
  let connections = map readConnection rows
  let paths = completePath canVisitT1 connections ["start"]
  print "Task 1"
  print $ length paths

  let paths = completePath canVisitT2 connections ["start"]
  print "Task 2"
  -- mapM_ (putStrLn) (map (foldl joinstr "" . reverse) paths)
  print $ length paths
  -- print paths
