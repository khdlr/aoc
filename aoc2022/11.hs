import Text.Read
import Data.List
import Data.List.Split
import Debug.Trace

data Test = Test Int Int Int
data Monkey = Monkey [Int] (Int -> Int) Test Int

parseOperation :: String -> (Int -> Int)
parseOperation opLine = evalOp l op r
  where
    [l, op, r] = case stripPrefix "  Operation: new =" opLine of
      Just text -> words text

evalOp :: String -> String -> String -> Int -> Int
evalOp left op right value = l `operation` r
  where
    l = case left of
      "old" -> value
      _ -> read left
    r = case right of
      "old" -> value
      _ -> read right
    operation = case op of
      "*" -> (*)
      "+" -> (+)
      "-" -> (-)

evalTest :: Test -> Int -> Int
evalTest (Test num t f) it | it `mod` num == 0 = t
                           | otherwise = f

parseMonkey :: [String] -> Monkey
parseMonkey [head, itemList, opDesc, test1, test2, test3] = Monkey items op test 0
  where
    items = case stripPrefix "  Starting items: " itemList of
      Just its -> map read . splitOn ", " $ its
    op = parseOperation opDesc
    test = Test testDiv testThrow1 testThrow2
    testDiv = case stripPrefix "  Test: divisible by " test1 of
      Just num -> read num
    testThrow1 = case stripPrefix "    If true: throw to monkey " test2 of
      Just num -> read num
    testThrow2 = case stripPrefix "    If false: throw to monkey " test3 of
      Just num -> read num

takeTurn :: [Monkey] -> Int -> [Monkey]
takeTurn monkeys i = map nextMonkey . withIndex $ monkeys
  where
    Monkey preItems op test insp = monkeys !! i
    items = map ((`div` 3) . op) preItems
    nextMonkey (idx, Monkey pItems pOp pTest pInsp) = Monkey nItems pOp pTest nInsp
      where
        nInsp | idx == i = pInsp + length preItems
              | otherwise = pInsp
        nItems | idx == i   = []
               | otherwise  = pItems ++ filter (\it -> evalTest test it == idx) items

takeReducedTurn :: Int -> [Monkey] -> Int -> [Monkey]
takeReducedTurn lcm monkeys i = map nextMonkey . withIndex $ monkeys
  where
    Monkey preItems op test insp = monkeys !! i
    items = map ((`mod` lcm) . op) preItems
    nextMonkey (idx, Monkey pItems pOp pTest pInsp) = Monkey nItems pOp pTest nInsp
      where
        nInsp | idx == i = pInsp + length preItems
              | otherwise = pInsp
        nItems | idx == i   = []
               | otherwise  = pItems ++ filter (\it -> evalTest test it == idx) items

withIndex :: [a] -> [(Int, a)]
withIndex = zip [0..]

loadMonkeys :: IO [Monkey]
loadMonkeys = do
  content <- readFile "11.dat"
  let
    monkeyDescs = map lines . splitOn "\n\n" $ content
  return (map parseMonkey monkeyDescs)

inspections (Monkey _ _ _ insp) = insp

main = do
  monkeys <- loadMonkeys

  let
    round = flip (foldl takeTurn ) [0..length monkeys-1]
    rounds = iterate round monkeys

    extractDivisor (Monkey _ _ (Test divisor _ _) _) = divisor
    modulus = foldl1 lcm (map extractDivisor monkeys)

    crazyRound = flip (foldl (takeReducedTurn modulus)) [0..length monkeys-1]
    crazyRounds = iterate crazyRound monkeys

  putStrLn "Task 1"
  print . product . take 2 . sortBy (flip compare) . map inspections $ rounds !! 20


  putStrLn "Task 2"
  print . product . take 2 . sortBy (flip compare) . map inspections $ crazyRounds !! 10000
