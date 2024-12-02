-- {-# LANGUAGE LambdaCase #-}
module Parser where
import qualified Data.Bifunctor as Bifunctor
import Data.Char
import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad (void, liftM, ap)

newtype Parser a = Parser
  { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  p >>= f = Parser $ \inp -> case parse p inp of
    Nothing -> Nothing
    Just (a, rest) -> parse (f a) rest

  return = result

instance Alternative Parser where
  empty = nothing
  p1 <|> p2 = Parser $ \inp -> case parse p1 inp of
    Just res -> Just res
    Nothing -> parse p2 inp

result :: a -> Parser a
result val = Parser $ \inp -> Just (val, inp)

nothing :: Parser a
nothing = Parser $ const Nothing

item :: Parser Char
item = Parser parseItem
  where
    parseItem "" = Nothing
    parseItem (x:xs) = Just (x, xs)

satisfies :: (Char -> Bool) -> Parser Char
satisfies predicate = item >>= \x -> if predicate x then result x else nothing

char :: Char -> Parser Char
char c = satisfies (==c)

digit :: Parser Char
digit = satisfies isDigit

string :: String -> Parser String
string "" = result ""
string (x:xs) =
  char x >> string xs >> result (x:xs)

many :: Parser a -> Parser [a]
many p = do
  x  <- p
  xs <- many p
  return (x:xs)
  <|> return []

some :: Parser a -> Parser [a]
some p = do
  x <- p
  xs <- many p
  return (x:xs)

nat :: Parser Int
nat = read <$> some digit

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = open *> p <* close

spaces :: Parser ()
spaces = void $ many $ satisfies isSpace

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = do
  x <- p
  xs <- many (sep >> p)
  return (x : xs)
  <|> return []

