import Data.Char
import Data.List
import Data.List.Split (splitOn)
import Parser
import Control.Applicative ((<|>))
import Debug.Trace

data Element = Num Int | List [Element] deriving (Eq)
instance Show Element where
  show (Num i) = show i
  show (List []) = "[]"
  show (List xs) = "[" ++ (foldl1 join . map show $ xs) ++ "]"

instance Ord Element where
  Num l `compare` Num r = l `compare` r
  Num l `compare` List r = List [Num l] `compare` List r
  List l `compare` Num r = List l `compare` List [Num r]
  List [] `compare` List [] = EQ
  List [] `compare` List (r:rs) = LT
  List (l:ls) `compare` List [] = GT
  List (l:ls) `compare` List (r:rs) = case l `compare` r of
      EQ -> List ls `compare` List rs
      LT -> LT
      GT -> GT


join :: String -> String -> String
join a b = a ++ ", " ++ b

listInt :: Parser Element
listInt = Num <$> nat

list :: Parser Element
list = char '[' *> content <* char ']'
  where content = List <$> (spaces *> (list <|> listInt) <* spaces) `sepBy` char ','

readLine :: String -> Element
readLine line = parsed
  where Just (parsed, _) = parse list line


loadData :: IO [(Element, Element)]
loadData = do
  content <- readFile "13.dat"
  let
    pairs = map lines $ splitOn "\n\n" content
    readPair [a, b] = (readLine a, readLine b)
    allData = map readPair pairs
  return allData

correct :: (Element, Element) -> Bool
correct (a, b) = a <= b

withIndex :: [a] -> [(Int, a)]
withIndex = zip [1..]

main = do
  pairs <- loadData
  let
    out = sum . map fst . filter (\(i, (a, b)) -> a <= b) . withIndex $ pairs
  putStrLn "Task 1"
  print out
  
  putStrLn "Task 2"
  let
    div1 = List [List [Num 2]]
    div2 = List [List [Num 6]]
    allPackets = div1 : div2 : concatMap (\(a, b) -> [a, b]) pairs
    sorted = sort allPackets

    Just idx1 = fmap (+1) $ div1 `elemIndex` sorted
    Just idx2 = fmap (+1) $ div2 `elemIndex` sorted
  print (idx1 * idx2)
