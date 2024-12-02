import Data.List

tokenscore :: Char -> Char -> Int
tokenscore _ 'X' = 1
tokenscore _ 'Y' = 2
tokenscore _ 'Z' = 3

gamescore :: Char -> Char -> Int
gamescore 'A' 'Y' = 6
gamescore 'B' 'Z' = 6
gamescore 'C' 'X' = 6
gamescore 'A' 'X' = 3
gamescore 'B' 'Y' = 3
gamescore 'C' 'Z' = 3
gamescore _ _ = 0

scoreline :: String -> Int
scoreline [l, ' ', r] = gamescore l r + tokenscore l r


gamescore2 :: Char -> Char -> Int
gamescore2 _ 'X' = 0
gamescore2 _ 'Y' = 3
gamescore2 _ 'Z' = 6

tokenscore2 :: Char -> Char -> Int

tokenscore2 'A' 'X' = 3  -- Elf plays R, we lose -> S
tokenscore2 'B' 'X' = 1  -- Elf plays P, we lose -> R
tokenscore2 'C' 'X' = 2  -- Elf plays S, we lose -> P

tokenscore2 'A' 'Y' = 1  -- Elf plays R, we draw -> R
tokenscore2 'B' 'Y' = 2  -- Elf plays P, we draw -> P
tokenscore2 'C' 'Y' = 3  -- Elf plays S, we draw -> S

tokenscore2 'A' 'Z' = 2  -- Elf plays R, we win -> P
tokenscore2 'B' 'Z' = 3  -- Elf plays P, we win -> S
tokenscore2 'C' 'Z' = 1  -- Elf plays S, we win -> R


scoreline2 :: String -> Int
scoreline2 [l, ' ', r] = gamescore2 l r + tokenscore2 l r


main = do
  content <- readFile "02.dat"
  let scores = map scoreline (lines content)
  print ("task 1: " ++ (show . sum $ scores))

  let scores2 = map scoreline2 (lines content)
  print ("task 2: " ++ (show . sum $ scores2))
