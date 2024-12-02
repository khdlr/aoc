import Data.List
import Data.List.Split


timeToMarker :: Int -> [Char] -> Int
timeToMarker n signal
  | (length . nub . take n $ signal) == n  = n
  | otherwise = 1 + timeToMarker n (tail signal)


main = do
  content <- readFile "06.dat"
  let signals = lines content

  print "Task 1"
  print . show . map (timeToMarker 4) $ signals

  print "Task 2"
  print . show . map (timeToMarker 14) $ signals

