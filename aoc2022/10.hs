import Data.List
import Data.List.Split

applyCommand :: [Int] -> String -> [Int]
applyCommand inputs command = case stripPrefix "addx " command of
  Just num -> [input, input + operand]
    where
      operand = read num
  Nothing  -> [input] -- noop
  where
    input = last inputs

sieve :: Int -> [a] -> [a]
sieve k = map head . takeWhile (not . null) . iterate (drop k)

withIndex :: [a] -> [(Int, a)]
withIndex = zip [1..]

asList :: (a, a) -> [a]
asList (a, b) = [a, b]

sprite :: (Int, Int) -> Char
sprite (cycle, register)
  | abs (wrappedCycle - register) <= 1 = '#'
  | otherwise = '.'
  where wrappedCycle = (cycle-1) `mod` 40

main = do
  content <- readFile "10.dat"
  let
    commands = lines content
    cycles = concat . scanl applyCommand [1] $ commands

  putStrLn "Task 1"
  print . sum . map product . sieve 40 . drop 19 . map asList . withIndex $ cycles

  putStrLn "Task 2"
  mapM_ print . chunksOf 40 . map sprite . withIndex $ cycles
