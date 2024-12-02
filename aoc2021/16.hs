import Utils

hex2bin :: Char -> [Int]
hex2bin '0' = [0, 0, 0, 0]
hex2bin '1' = [0, 0, 0, 1]
hex2bin '2' = [0, 0, 1, 0]
hex2bin '3' = [0, 0, 1, 1]
hex2bin '4' = [0, 1, 0, 0]
hex2bin '5' = [0, 1, 0, 1]
hex2bin '6' = [0, 1, 1, 0]
hex2bin '7' = [0, 1, 1, 1]
hex2bin '8' = [1, 0, 0, 0]
hex2bin '9' = [1, 0, 0, 1]
hex2bin 'A' = [1, 0, 1, 0]
hex2bin 'B' = [1, 0, 1, 1]
hex2bin 'C' = [1, 1, 0, 0]
hex2bin 'D' = [1, 1, 0, 1]
hex2bin 'E' = [1, 1, 1, 0]
hex2bin 'F' = [1, 1, 1, 1]
hex2bin illegalToken = error ("Can't parse '" ++ [illegalToken] ++ "'")


bin2int :: [Int] -> Int
bin2int = foldl (\acc x -> 2 * acc + x) 0

splicePacket :: [Int] -> (Int, Int, [Int])
splicePacket bits = (version, typeID, content)
  where
    version = bin2int $ take 3 bits
    typeID  = bin2int $ take 3 . drop 3 $ bits
    content = drop 6 bits


decodePacket :: [Int] -> Int
decodePacket bits =
  0
  where
    version = take 3 bits

-- tokenize :: 
-- parsePackage :: 


main = do
  hexData <- readLine "16.dat"
  let bits = hexData >>= hex2bin 

  print bits
