import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.List.Split

data FSNode =
  File Int
  | Dir (HashMap String FSNode)
  deriving (Eq, Show)


addFile :: FSNode -> [String] -> Int -> FSNode
addFile (Dir oldTree) (currentpath : rest) size =
  Dir (HM.insert currentpath inserted oldTree)
  where
    inserted
      | null rest  = File size
      | otherwise  = addFile subtree rest size
    subtree = HM.findWithDefault (Dir HM.empty) currentpath oldTree

parseCommand :: ([String], FSNode) -> String -> ([String], FSNode)
parseCommand (_, tree) "$ cd /" = ([], tree)
parseCommand (path, tree) "$ ls" = (path, tree)
parseCommand (path, tree) "$ cd .." = (init path, tree)
parseCommand (path, tree) ('$':' ':'c':'d':' ':subdir) = (path++[subdir], tree)
parseCommand (path, tree) filestat
  | "dir" `isPrefixOf` filestat = (path, tree)
  | otherwise = (path, addFile tree (path ++ [filename]) (read filesize))
    where
      [filesize, filename] = words filestat

nodeSize :: FSNode -> Int
nodeSize (File n) = n
nodeSize (Dir dirmap) = sum . map nodeSize . HM.elems $ dirmap

dirsWithSize :: FSNode -> [([String], Int)]
dirsWithSize (Dir root) = ([], nodeSize (Dir root)) : childsizes
  where
    values = HM.toList root
    childMap (name, File sz) = []
    childMap (name, dir) = map (\(p, sz) -> (name:p, sz)) (dirsWithSize dir)
    childsizes = values >>= childMap

main = do
  content <- readFile "07.dat"
  let terminal = lines content

  print "Task 1"
  let
    (_, parsedTree) = foldl parseCommand ([], Dir HM.empty) terminal
    dirsizes = dirsWithSize parsedTree
    currentlyUsed = snd . head $ dirsizes
  print . show . sum . map snd . filter ((<=100000) . snd) $ dirsizes

  print "Task 2"
  let filt x = 70000000 - currentlyUsed + x >= 30000000
  print . show . minimum . map snd . filter (filt . snd) $ dirsizes

