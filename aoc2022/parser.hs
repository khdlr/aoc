


newtype Parser i o =
  Parser { runParser :: i -> Maybe (i, o) }

instance Functor (Parser i) where
  fmap f parser = Parser $ fmap (fmap f) . runParser parser

satisfy :: (a -> Bool) -> Parser [a] a
satisfy predicate = Parser fun
  where
    fun chr = case chr of
      (x:xs) | predicate x -> Just (xs, x)
      _                    -> Nothing

digit :: Parser String Int
digit = digitToInt <$> satisfy isDigit

jInt :: Parser String Int
jInt = (\d ds -> digitsToNumber 10 0 (d:ds)) <$> digit19 <*> digits 

digit19 :: Parser String Int
digit19 = digitToInt <$> satisfy (\x -> isDigit x && x /= '0')

digits :: Parser String [Int]
digits = some digit
