import Utils
import Data.Set (Set)
import qualified Data.Set as Set

bigCup :: Ord a => [Set a] -> Set a
bigCup [] = Set.empty
bigCup (x:xs) = x `Set.union` (bigCup xs)

bigCap :: Ord a => [Set a] -> Set a
bigCap [x] = x
bigCap (x:xs) = x `Set.intersection` (bigCap xs)

decodeTokens :: ([Set Char], [Set Char]) -> [Int]
decodeTokens (digits, tokens) =
  map getToken tokens
  where
    getToken segments
      | segments == bigCup [a, b, c,    e, f, g]  = 0
      | segments == bigCup [      c,       f   ]  = 1
      | segments == bigCup [a,    c, d, e,    g]  = 2
      | segments == bigCup [a,    c, d,    f, g]  = 3
      | segments == bigCup [   b, c, d,    f   ]  = 4
      | segments == bigCup [a, b,    d,    f, g]  = 5
      | segments == bigCup [a, b,    d, e, f, g]  = 6
      | segments == bigCup [a,    c,       f   ]  = 7
      | segments == bigCup [a, b, c, d, e, f, g]  = 8
      | segments == bigCup [a, b, c, d,    f, g]  = 9
      | otherwise                                 = error (errormsg segments)
    -- mkstr name s a 
    --   | Set.null (a `Set.intersection` s) = ""
    --   | otherwise                         = name
    mkstr name s a = name ++ "->" ++ (show a) ++ "\n"
    errormsg s = (show s) ++ "\n" ++
                   (mkstr "a" s a) ++
                   (mkstr "b" s b) ++
                   (mkstr "c" s c) ++
                   (mkstr "d" s d) ++
                   (mkstr "e" s e) ++
                   (mkstr "f" s f) ++
                   (mkstr "g" s g) ++
                   (mkstr "adg" s adg) ++
                   (mkstr "abfg" s abfg) ++
                   (mkstr "g" s g)
    allWith n = filter (\x -> length x == n) digits
    theOneWith n = head $ allWith n
    intersectAllWith = bigCap . allWith
    unionAllWith = bigCup . allWith
    a = theOneWith 3 `Set.difference` theOneWith 2
    b = abfg `Set.difference` (adg `Set.union` f)
    c = cf `Set.difference` abfg
    d = adg `Set.difference` abfg
    e = (theOneWith 7) `Set.difference` (abfg `Set.union` c `Set.union` d)
    g = adg `Set.difference` (a `Set.union` d)
    f = cf `Set.difference` c
    cf   = theOneWith 2
    adg  = bigCap . allWith $ 5
    abfg = bigCap . allWith $ 6

parseLine :: String -> ([Set Char], [Set Char])
parseLine line = (digits, target)
  where
    digits = map Set.fromList $ take 10 captures
    target = map Set.fromList $ drop 11 captures
    captures = words line

digitsToNum :: [Int] -> Int
digitsToNum [a,b,c,d] = 1000*a + 100*b + 10 * c + d
digitsToNum _ = 0

main = do
  displays <- readLines "8.dat"
  let puzzles = map parseLine displays
  let solutions = map decodeTokens puzzles

  let digits = puzzles >>= decodeTokens
  let task1 = length . (filter (\x -> elem x [1,4,7,8])) $ digits 

  putStrLn . show $ task1

  let numbers = map digitsToNum solutions
  let task2 = sum numbers

  putStrLn . show $ task2


