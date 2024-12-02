import Text.Regex.PCRE
import Text.Regex.PCRE.String ()
import Data.List
import Debug.Trace
import Data.Bifunctor (first)

ore i = [i, 0, 0, 0]
clay i = [0, i, 0, 0]
obsidian i = [0, 0, i, 0] 
geode i = [0, 0, 0, i]

valid :: [Int] -> Bool
valid = all (>0)

add :: [Int] -> [Int] -> [Int]
add = zipWith (+)

minus :: [Int] -> [Int] -> [Int]
minus = zipWith (-)

scl :: Int -> [Int] -> [Int]
scl a = map (a*)

parseBlueprint :: String -> [[Int]]
parseBlueprint line = map parseRecipe matches
  where
    matches = map tail (line =~ regex)
    regex = "Each ([a-z]+) robot costs (\\d+) ([a-z]+)(?: and (\\d+) ([a-z]+))*"

parseRecipe :: [String] -> [Int]
parseRecipe (name:ing) = ingredients ing
  where
    ingredients [] = [0, 0, 0, 0]
    ingredients ("":rest) = ingredients rest
    ingredients (num:"ore":rest) = ore (read num) `add` ingredients rest
    ingredients (num:"clay":rest) = clay (read num) `add` ingredients rest
    ingredients (num:"obsidian":rest) = obsidian (read num) `add` ingredients rest
    ingredients (num:"geode":rest) = geode (read num) `add` ingredients rest

readData :: IO [[[Int]]]
readData = do
  content <- readFile "19.dat"
  return . map parseBlueprint . lines $ content

allGT :: Ord a => [a] -> [a] -> Bool
allGT a b = all (uncurry (>=)) $ zip a b

unit :: Int -> [Int]
unit i = map (fromEnum . (==i)) [0..3]

-- plans :: [[Int]] -> [([Int], Int)]
plans :: Int -> [[Int]] -> [([Int], ([Int], [Int]))]
plans time blueprints = nextPlans (time, [1, 0, 0, 0], [0, 0, 0, 0])
  where
    maxCosts = map maximum $ transpose blueprints

    nextPlans :: (Int, [Int], [Int]) -> [([Int], ([Int], [Int]))]
    nextPlans state = concatMap iterateOrEmpty (3:shouldInvestigate)
      where
        (_, factories, _) = state
        shouldInvestigate = map (\(a, _, _) -> a) .
                            filter (\(_, have, need) -> have < need) $
                            zip3 [0..3] factories maxCosts

        iterateOrEmpty i
          | t <= 0 = []
          | null furtherPlans = [([i], finalState)]
          | otherwise = furtherPlans
          where
            furtherPlans = map (first (i:)) (nextPlans (t, f, r))
            (t, f, r) = stateAfter state i
            finalState = (f, r `add` (t `scl` f))

    stateAfter :: (Int, [Int], [Int]) -> Int -> (Int, [Int], [Int])
    stateAfter (time, factories, resources) planned
      | resources `allGT` costs = (time - 1, builtFac, steppedRes `minus` costs)
      | time <= 0 = (-1, factories, steppedRes)
      | otherwise = stateAfter (time - 1, factories, steppedRes) planned
      where
        costs = blueprints !! planned
        steppedRes = resources `add` factories
        builtFac = factories `add` unit planned


main = do
  blueprints <- readData

  let
    qualityLevel b = maximum . map (last . snd . snd) $ plans 24 b
    bp1 = head blueprints
    levels = map qualityLevel blueprints

  putStrLn "Part 1"
  putStrLn "Levels:"
  print levels
  putStrLn ("Answer: " ++ (show . sum $ zipWith (*) levels [1..]))

  let
    qualityLevel' b = maximum . map (last . snd . snd) $ plans 32 b
    levels' = map qualityLevel' (take 3 blueprints)

  putStrLn "Part 2"
  putStrLn "Levels:"
  print levels'
  putStrLn ("Answer: " ++ show (product levels'))
