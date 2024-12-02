import Debug.Trace

parseChr :: Char -> Int
parseChr '0' = 0
parseChr '1' = 1
parseChr '2' = 2
parseChr '-' = -1
parseChr '=' = -2

writeChr :: Int -> Char
writeChr 0 = '0'
writeChr 1 = '1'
writeChr 2 = '2'
writeChr (-1) = '-'
writeChr (-2) = '='

parseSNAFU :: String -> Int
parseSNAFU = foldl (\acc c -> parseChr c + 5 * acc) 0

writeSNAFU :: Int -> String
writeSNAFU i
  | rest == 0 = [writeChr digit]
  | otherwise = writeSNAFU rest ++ [writeChr digit]
  where
    rest = (i - digit) `div` 5
    digit = ((i + 2) `mod` 5) - 2

main = do
  numbers <- map parseSNAFU . lines <$> readFile "25.dat"
  let
    sol1 = sum numbers
  putStrLn . writeSNAFU $ sol1
  -- mapM_ (putStrLn . writeSNAFU) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 2022, 12345]

