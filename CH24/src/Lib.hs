module Lib where

import Text.Trifecta
import Control.Applicative ((<|>))
import Data.Ratio ((%))

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

-- 24.3 Understanding parsing process
--     Exercises: Parsing Practice
--         1.
one' = one >> eof
--  parseString one' mempty "123"
oneTwo' = oneTwo >> eof
--  parseString oneTwo' mempty "123"
--         2.
someNumbers :: Parser String
someNumbers = (string "123" <|> string "12" <|> string "1") >> stop
--         3.
string' :: CharParsing m => String -> m String
string' = traverse char
-- parseString (string "awesome") mempty "awesome"
-- parseString (string' "awesome") mempty "awesome"

-- 24.4 Parsing fractions
--     Exercise: Unit of Success
parseNumbers :: Parser Integer
parseNumbers = do
  i <- integer
  eof
  return i
-- Alternative using bind & lambda
-- parseNumbers = integer >>= \i -> eof >> return i

-- 24.6 Alternative
--     Exercise: Try try
virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

ior :: Parser (Either Integer Rational)
ior = Right <$> try virtuousFraction <|> Left <$> try decimal
