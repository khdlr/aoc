import Data.List.Split
import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HashMap
import Debug.Trace

readOperations :: [String] -> HashMap String String
readOperations = HashMap.fromList . map readLine
  where
    readLine line = let [name, proc] = splitOn ": " line in (name, proc)

evaluateRoot :: HashMap String String -> Int
evaluateRoot monkeys = eval "root"
  where
    eval :: String -> Int
    eval name = result
      where
        result = case fmap words (monkeys !? name) of
          Nothing -> error "Monkey not found"
          Just [num] -> read num
          Just [left, op, right] -> eval left `operator` eval right
            where
             operator = case op of
                "*" -> (*)
                "+" -> (+)
                "-" -> (-)
                "/" -> div

data Operator = Add | Sub | Mul | Div
  deriving (Eq)
instance Show Operator where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

inverseOp :: Operator -> Operator
inverseOp op = case op of
  Add -> Sub
  Sub -> Add
  Mul -> Div
  Div -> Mul

data Entry = Num Int
  | Binary Operator Entry Entry
  | Eq Entry Entry
  | Human
  deriving (Eq)
instance Show Entry where
  show Human = "?"
  show (Binary op a b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
  show (Eq a b) = "(" ++ show a ++ " = " ++ show b ++ ")"
  show (Num n) = show n

eval :: Operator -> Int -> Int -> Int
eval Add = (+)
eval Sub = (-)
eval Mul = (*)
eval Div = div

reduce :: Entry -> Entry
reduce (Eq a b) = simplifyEq (reduce a) (reduce b)
reduce (Binary op (Num a) (Num b)) = Num (eval op a b)
reduce (Binary op a b) = Binary op (reduce a) (reduce b)
reduce other = other

simplifyEq :: Entry -> Entry -> Entry
simplifyEq (Num a) (Binary Add (Num b) c) = Num (eval Sub a b) `Eq` c
simplifyEq (Num a) (Binary Mul (Num b) c) = Num (eval Div a b) `Eq` c
simplifyEq (Num a) (Binary Sub (Num b) c) = Num (eval Sub b a) `Eq` c
simplifyEq (Num a) (Binary Div (Num b) c) = Num (eval Div b a) `Eq` c
simplifyEq (Num a) (Binary op b (Num c)) = Num (eval (inverseOp op) a c) `Eq` b
simplifyEq b (Num a) = simplifyEq (Num a) b
simplifyEq a b = Eq a b

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint fn x
  | next == x = x
  | otherwise = fixpoint fn next
  where
    next = fn x

parseTree :: HashMap String String -> Entry
parseTree monkeys = Eq (parse rootLeft) (parse rootRight)
  where
    Just [rootLeft, _, rootRight] = fmap words (monkeys !? "root")
    parse :: String -> Entry
    parse "humn" = Human
    parse name = result
      where
        result = case fmap words (monkeys !? name) of
          Nothing -> error "Monkey not found"
          Just [num] -> Num (read num)
          Just [left, op, right] -> Binary operator (parse left) (parse right)
            where
             operator = case op of
                "+" -> Add
                "-" -> Sub
                "*" -> Mul
                "/" -> Div

main = do
  monkeys <- readOperations . lines <$> readFile "21.dat"
  print $ evaluateRoot monkeys
  let
    tree = parseTree monkeys
  print tree
  print (fixpoint reduce tree) 
