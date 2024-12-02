import Utils
import Data.HashMap (Map)
import qualified Data.HashMap as Map
import Debug.Trace

type Rule = ((Char, Char), Char)
type Rules = Map (Char, Char) Char
type PairCounts = Map (Char, Char) Int

parseRule :: String -> Rule
parseRule row = ((left, right), result)
  where
    [[left, right], _, [result]] = words row

substituteIfPossible :: Rules -> (Char, Char) -> String
substituteIfPossible rules key =
  case insert of
    Just char -> [fst key, char]
    Nothing   -> [fst key] 
  where insert = Map.lookup key rules

substitutionStep :: Rules -> String -> String
substitutionStep rules polymer =
  (pairs >>= substituteIfPossible rules) ++ [last polymer]
  where
    pairs = zip polymer (drop 1 polymer)

evolveCounts :: Rules -> (Char, Char) -> Int -> PairCounts
evolveCounts rules pair@(l,r) count =
  case lookup of
    Nothing   -> Map.fromList [(pair, count)]
    Just new  -> Map.fromList [((l,new), count), ((new,r), count)]
  where lookup = Map.lookup pair rules

aggStep :: Rules -> PairCounts -> PairCounts
aggStep rules counts =
  Map.unionsWith (+) evolvedCounts
  where
    evolvedCounts = Map.elems $ Map.mapWithKey (evolveCounts rules) counts

letterCount :: PairCounts -> Map Char Int
letterCount counts =
  Map.unionsWith (+) cutoffCounts
  where
    extract k a = Map.fromList [(fst k, a)]
    cutoffCounts = Map.elems $ Map.mapWithKey extract counts

main = do
  rows <- readLines "14.dat"
  let polymer = head rows ++ "$"
  let rules = Map.fromList $ map parseRule (drop 2 rows)

  let initCounts = countValues $ zip polymer (drop 1 polymer)
  let pairCounts = iterate (aggStep rules) initCounts
  let steps = map (Map.elems . letterCount) pairCounts

  let counts1 = steps !! 10
  print $ maximum counts1 - minimum counts1

  let counts2 = steps !! 40
  print $ maximum counts2 - minimum counts2
