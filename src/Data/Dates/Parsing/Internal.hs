{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Dates.Parsing.Internal where

import Data.Char           (digitToInt, isDigit, toUpper, toLower, toUpper)
import Data.Hourglass
import Data.List           (isPrefixOf, lookup)
import Data.Maybe          (fromJust, catMaybes)
import Text.Megaparsec
import Text.Read (readMaybe)
import Data.String (fromString, IsString)
import Text.Megaparsec.Char (string', string, char, space, digitChar)
import Control.Monad (void)
import Data.CaseInsensitive (FoldCase)

-- | Parsers the parser at least once, but no more than n times.
takeN1 :: MonadParsec e s m => Int -> m a -> m [a]
takeN1 0 _ = error "n must not be 0!"
takeN1 1 parser = (:[]) <$> parser
takeN1 n parser = do
    v <- parser
    rest <- takeN1 (n - 1) parser <|> pure []
    pure $ v : rest

-- | Parse natural number of N digits which is not greater than M
number :: (MonadFail m, MonadParsec e s m, Token s ~ Char, Integral a, Show a)
       => Int -- ^ Number of digits
       -> a   -- ^ Maximum value
       -> m a
number n m = do
  maybeT <- readMaybe <$> takeN1 n digitChar
  case fromIntegral <$> maybeT of
    Just t | t <= m -> pure t
    _ -> fail $ "Couldn't parse into number with parameters: " ++ show n ++ "," ++ show m

pYear :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => m Int
pYear = do
    n <- try pYearNormal <|> pYearAny
        -- Assume two digit years are after 2000.
        -- TODO: Update in 82 years (2018-05-03).
    pure $ if n < 2000 && n < 100 && n >= 10 then n + 2000 else n

pYearNormal :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => m Int
pYearNormal = do
    n <- read <$> some digitChar

    notFollowedBy (try (space >> void yearAbbreviations) <|> void digitChar)

    pure n

readNum :: (Num a, MonadParsec e s m, Token s ~ Char) => m a
readNum = do
    isNegative <- optional $ char '-'
    digits <- some digitChar

    let sign = maybe 1 (const (-1)) isNegative

    pure $ sign * fromInteger (read digits)

yearAbbreviations :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => m (Tokens s)
yearAbbreviations = choice $ map (try . abbParser) ["BCE", "AD", "CE", "BC"]
    where
        abbParser :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => String -> m (Tokens s)
        abbParser abbr = parseAs (map (string' . fromString) $ makeAbbr abbr) (fromString abbr)

makeAbbr :: String -> [String]
makeAbbr abb = [abb, foldl (\cur n -> cur ++ [n] ++ ".") "" abb]

pYearAny :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => m Int
pYearAny = do
    space
    -- Allow abbreviations before or after.
    prefix <- optional yearAbbreviations

    space
    n <- readNum
    space

    suffix <- optional yearAbbreviations

    let isBC = case catMaybes [prefix, suffix] of
                (abb:_) -> abb == fromString "BC" || abb == fromString "BCE"
                [] -> False

    pure $ if isBC then -n else n

monthAssoc :: [(String, Month)]
monthAssoc = [("january", January), ("jan", January), ("february", February), ("feb", February),
              ("march", March), ("mar", March), ("april", April), ("apr", April),
              ("may", May), ("june", June), ("jun", June), ("july", July), ("july", July),
              ("august", August), ("aug", August), ("september", September), ("sept", September),
              ("october", October), ("oct", October), ("november", November), ("nov", November),
              ("december", December), ("dec", December)]

parseAs :: (MonadParsec e s m, Token s ~ Char) => [m b] -> a -> m a
parseAs options a = do
    _ <- choice $ map try options
    optional $ char '.'
    pure a

pMonthName :: (MonadParsec e s m, Token s ~ Char, FoldCase (Tokens s), IsString (Tokens s)) => m Month
pMonthName = do
    monthName <- choice $ map (\(name,_) -> try $ (string' (fromString name) *> optional (char '.') *> pure name)) monthAssoc

    -- Safe because month names come from monthAssoc
    return $ fromJust $ lookup monthName monthAssoc

pMonth :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => m Month
pMonth = try (toEnum . pred <$> number 2 12) <|>
         pMonthName

pDay :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m Int
pDay = number 2 31

uppercase :: String -> String
uppercase = map toUpper

-- | Case-insensitive version of 'isPrefixOf'
isPrefixOfI :: String -> String -> Bool
p `isPrefixOfI` s = uppercase p `isPrefixOf` uppercase s

-- | Use a data type's Bounded, Enum and Show instances to determine if the
-- given string uniquely matches a constructor. The comparison is
-- case-insensitive and starts from the beginning of the strings (so a partial
-- constructor name can still match if there are enough characters for a
-- unique match)
--
-- For example:
--
-- @
--  data Things = Foo | Bar | Baz deriving (Bounded, Enum, Show)
--
--  -- Right Foo
--  uniqFuzzyMatch "f" :: Either [Things] Things
--
--  -- Left [Bar, Baz]
--  uniqFuzzyMatch "ba" :: Either [Things] Things
-- @
uniqFuzzyMatch :: (Bounded a, Enum a, Show a)
               => String
               -> Either [a] a -- ^ Either collection of matches or the unique match
uniqFuzzyMatch n =
    case matches of
        [match] -> Right match
        _ -> Left matches
  where
    possibilities = [minBound..maxBound]
    matches = filter (isPrefixOfI n . show) possibilities

