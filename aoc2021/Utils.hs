module Utils where

import Debug.Trace
import Data.Hashable (Hashable)
import Data.HashMap (Map)
import qualified Data.HashMap as Map

traceid :: Show a => a -> a
traceid a = trace (show a) a

trtag :: Show a => String -> a -> a
trtag tag a = trace (tag ++ ": " ++ show a) a

readline :: [Char] -> [Int]
readline = map (\x -> read [x] :: Int)

splitOrAppend :: Char -> [[Char]] -> Char -> [[Char]]
splitOrAppend delim acc chr
  | delim == chr  = acc ++ [[]]
  | otherwise     = init acc ++ [last acc ++ [chr]]

splitStr :: Char -> [Char] -> [[Char]]
splitStr delim = foldl (splitOrAppend delim) [[]]

readCommaList :: String -> IO [Int]
readCommaList filename = do
  content <- readFile filename
  let res = map (read :: [Char] -> Int) (splitStr ',' content)
  return res

readLines :: String -> IO [String]
readLines filename = do
  content <- readFile filename
  return (lines content)

readLine :: String -> IO String
readLine filename = do
  content <- readFile filename
  return . head . lines $ content


parseInt = read :: String -> Int

countValues :: (Hashable a, Ord a) => [a] -> Map a Int
countValues [] = Map.empty
countValues (x:xs) = Map.insert x newCount oldMap
  where
    newCount = 1 + Map.findWithDefault 0 x oldMap
    oldMap = countValues xs

