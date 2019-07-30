module Lib where

import Text.Trifecta
import Control.Applicative ((<|>))
import Data.Ratio ((%))
import Data.Char(digitToInt)
import Data.Word
import Data.Bits

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
-- parseString ior mempty "1223/1"
-- parseString ior mempty "1223"
-- parseString ior mempty "1223/0"

-- 24.11 Chapter Excercises
--     1. SemVer
-- Eq & Ord for NmuberOrString could be derived.
data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Show)

instance Eq NumberOrString where
  (==) (NOSI i) (NOSI i') = i == i'
  (==) (NOSS s) (NOSS s') = s == s'
  (==) _ _ = False

instance Ord NumberOrString where
  compare (NOSI i) (NOSI i') = compare i i'
  compare (NOSS s) (NOSS s') = compare s s'
  compare (NOSI _) _         = LT
  compare _        (NOSI _)  = GT

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

-- Eq for SemVar could also be derived
data SemVer =
  SemVer Major Minor Patch Release Metadata deriving (Show)

instance Eq SemVer where
  (==) (SemVer ma mi pa re me) (SemVer ma' mi' pa' re' me') =
    ma == ma' && mi == mi' && pa == pa' && re == re' && me == me'

instance Ord SemVer where
  compare (SemVer ma mi pa re _) (SemVer ma' mi' pa' re' _) =
    compare ma ma' <> compare mi mi' <> compare pa pa' <> compare re re'

parseSemVer :: Parser SemVer
-- Monad style impl with do syntax
parseSemVer = do
  major <- integer
  minor <- char '.' >> integer
  patch <- char '.' >> integer
  release <- (char '-' >> parseNumberOrString `sepBy1` char '.') <|> return []
  metadata <- (char '+' >> parseNumberOrString `sepBy1` char '.') <|> return []
  return $ SemVer major minor patch release metadata

-- Applicative style impl
parseSemVer' :: Parser SemVer
parseSemVer' = SemVer
               <$> integer
               <*> (char '.' *> integer)
               <*> (char '.' *> integer)
               <*> (char '-' *> parseNumberOrString `sepBy1` char '.' <|> mempty)
               <*> (char '+' *> parseNumberOrString `sepBy1` char '.' <|> mempty)

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = NOSI <$> integer <* notFollowedBy alphaNum
                     <|> NOSS <$> some alphaNum

--     2. Parse positive integer
parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9']

base10Integer :: Parser Integer
base10Integer = (fromIntegral.digitToInt <$> parseDigit) `chainl1` return (\acc x -> acc * 10 + fromIntegral x)

base10Integer' :: Parser Integer
base10Integer' = digitsToBase10Int.fmap digitToInt <$> some parseDigit
                 where digitsToBase10Int = foldl (\acc x -> acc * 10 + fromIntegral x) 0

--     3. Parse aso negative integer
base10Integer'' :: Parser Integer
base10Integer'' = do
  sig <- oneOf "+-" <|> return '+'
  i <- base10Integer
  if sig == '-' then return (negate i) else return i

base10Integer''' :: Parser Integer
base10Integer''' = ((char '-' >> return negate) <|> (char '+' >> return id) <|> return id) <*> base10Integer

--     4. US/Canada phone Number
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  optional $ (digit >> char '-') <|> char '('
  numberingPlanArea <- count 3 digit
  optional $ char '-' <|> (char ')' >> char ' ')
  exchange <- count 3 digit
  optional $ char '-'
  lineNumber <- count 4 digit
  return $ PhoneNumber (read numberingPlanArea) (read exchange) (read lineNumber)

-- Applicative style. As there are too many tricks, it seems monad style is better.
parsePhone' :: Parser PhoneNumber
parsePhone' = PhoneNumber
  <$> (optional ((digit *> char '-') <|> char '(') *> (read <$> count 3 digit))
  <*> (optional (char '-' <|> (char ')' *> char ' ')) *> (read <$> count 3 digit))
  <*> (optional (char '-') *> (read <$> count 4 digit))

--     5. Log file TODO

--     6. IPv4 addresses
newtype IPAddress = IPAddress Word32 deriving (Eq, Ord, Show)
parseIPv4Address :: Parser IPAddress
parseIPv4Address = IPAddress . fromIntegral <$> (integer `chainl1` do {char '.'; return (\acc a->acc `shiftL` 8 .|. a )})
-- parseString parseIPv4Address mempty "172.16.254.1" -- Should be: Success (IPAddress 2886794753)
-- parseString parseIPv4Address mempty "204.120.0.15" -- Should be: Success (IPAddress 3430416399)

--     7. IPv6 addresses TODO
