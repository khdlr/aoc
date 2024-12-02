import Data.List
import Debug.Trace

underHalf xs n = takeWhile (<(n`div`2)) xs
xs `below` n = takeWhile (<=n) xs
between xs (a, b) = takeWhile (<=b) . dropWhile (<a) $ xs

-- "teilbar" Operator
(/.) :: Int -> Int -> Bool
a /. b = (a `mod` b) == 0

-- "nichttrivial teilbar" Operator
(/:) :: Int -> Int -> Bool
a /: b = (a `mod` b) == 0 && (a `div` a) `mod` 2 == 1

(gentleman, ladylike) = partition isGentleman [1..]
isGentleman n
  | n < 5                              = False -- (0)
  | n /. 5                             = True  -- (1)
  | n /. 7                             = False -- (2)
  | (n-1) /. 7 || (n+1) /. 7           = True  -- (3)
  | or [n /. (2*g) | g <- gentleman `below` (n `div` 2)] = False -- (4)
  | or [n /: l | l <- ladylike `between` (5, n `div` 3)] = True -- (5)
  | (n-3) /. 7 || (n+3) /. 7           = False -- (6)
  | otherwise                          = True  -- (7)
