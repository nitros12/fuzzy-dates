{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parse strings that aren't so precise
module Data.Dates.Parsing
  (
    Config (..)
  , DateTime (..)
  , DateInterval (..)
  , Time (..)
  , defaultConfig
  , defaultConfigIO
  , parseDate
  , parseDateTime
  , pAbsDateTime
  , pAbsDate
  , pDate
  , pDateTime
  , time
  , pDateInterval
  , weekdayToInterval
  , dateWeekDay
  , getStartOfThisWeek
  , getStartOfNextWeek
  , lastDate
  , nextDate
  , addInterval
  , negateInterval
  , minusInterval
  , dateInFormat
  , extractDates, extractDatesY, extractDatesConfig
  , extractDateTimes, extractDateTimesY, extractDateTimesConfig
  , extract
  ) where

import Control.Lens
import Control.Monad

import Data.Char                            (toLower)
import Data.Data                            (Data, Typeable)
import Data.Hourglass
import Data.List                            (intercalate, find)
import Data.Maybe (fromMaybe, catMaybes)
import Text.Megaparsec
import Text.Read (readMaybe)

import Time.System (dateCurrent)

import Data.Dates.Parsing.Internal
import Text.Megaparsec.Char (string', digitChar, letterChar, space, char, string)
import Data.String (IsString(fromString))
import Data.CaseInsensitive (FoldCase)
import Data.Int (Int64)
import Data.Functor (($>))

data DateInterval = Days Int
                  | Weeks Int
                  | Months Int
                  | Years Int
  deriving (Eq,Show,Data,Typeable)

data Config = Config
    { _now            :: DateTime -- ^ "Current" date/time, to use as base for relative dates
    , _startOfWeekDay :: WeekDay} -- ^ Which day of the week to consider the start day

makeLenses ''Config

defaultConfig :: DateTime -> Config
defaultConfig now' = Config
  {
    _now = now'
  , _startOfWeekDay = Monday
  }

defaultConfigIO :: IO Config
defaultConfigIO = defaultConfig <$> dateCurrent

-- | Weekday as interval from the configure start of the week
weekdayToInterval :: Config -> WeekDay -> DateInterval
weekdayToInterval c wd =
  Days (fromIntegral $ fromEnum wd - fromEnum (c^.startOfWeekDay))

getStartOfThisWeek :: Config -> DateTime
getStartOfThisWeek c = (c^.now) `minusInterval` weekdayToInterval c (dateWeekDay (c^.now))

getStartOfNextWeek :: Config -> DateTime
getStartOfNextWeek c = getStartOfThisWeek c `addInterval` Weeks 1

-- | Get weekday of given date.
dateWeekDay :: DateTime -> WeekDay
dateWeekDay = getWeekDay . timeGetDate

lookupMonth :: String -> Either [Month] Month
lookupMonth = uniqFuzzyMatch

time :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => m TimeOfDay
time = do
    h <- fromIntegral <$> number 2 23

    minsecs <- count' 0 2 $ do
      void (char ':' <|> char '.')
      number 2 59

    offset <- optional (space *> ampm)

    let (m, s) = case minsecs of
                   [] -> (0, 0)
                   [m] -> (m, 0)
                   (m : s : _) -> (m, s)

    -- It shouldn't be a 24 hour time, so just ignore offset, if any
    let offset' = if h > 12 then 0 else maybe 0 fromIntegral offset

    pure $ TimeOfDay (Hours $ h + offset') (Minutes m) (Seconds s) 0

ampm :: (MonadFail m, MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => m Int
ampm = (string' "am" $> 0) <|> (string' "pm" $> 12)

newtype DateFormat = DateFormat [(DatePart, String)]
data DatePart = D | M | Y
data DatePartVal = DV Int | MV Month | YV Int

datePart :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => DatePart -> m DatePartVal
datePart M = MV <$> pMonth
datePart D = DV <$> pDay
datePart Y = YV <$> pYear

isYV (YV _) = True
isYV _ = False

isMV (MV _) = True
isMV _ = False

isDV (DV _) = True
isDV _ = False

monthPart :: [DatePartVal] -> Month
monthPart = maybe January (\(MV m) -> m) . find isMV

dayPart :: [DatePartVal] -> Int
dayPart = maybe 1 (\(DV d) -> d) . find isDV

yearPart :: Int -> [DatePartVal] -> Int
yearPart year = maybe year (\(YV y) -> y) . find isYV

makeFormat :: String -> [DatePart] -> DateFormat
makeFormat sep parts = DateFormat $ zip parts $ repeat sep

dateInFormat :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => Int -> DateFormat -> m Date
dateInFormat year (DateFormat parts) = do
    partVals <- zipWithM go [1..] parts

    pure $ Date (yearPart year partVals) (monthPart partVals) (dayPart partVals)
    where
        go i (p, sep)
            -- The last one doesn't need to have a separator.
            | i == length parts = datePart p
            | otherwise = do
                v <- datePart p
                string (fromString sep)
                pure v

euroNumDate = makeFormat "." [D, M, Y]
writtenDate = DateFormat [(M, " "), (D, ","), (Y, "")]
americanDate = makeFormat "/" [M, D, Y]
dashDate = makeFormat "-" [Y, M, D]
strDate = makeFormat " " [D, M, Y]
spaceDate = makeFormat " " [D, M]
spaceDateMD = makeFormat " " [M, D]

dotDateMonth = makeFormat "." [D, M]
dashDateMonth = makeFormat "-" [M, D]
slashDateMonth = makeFormat "/" [M, D]

pAbsDateTime :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => Int -> m DateTime
pAbsDateTime year = do
    date <- pAbsDate year

    space
    optional $ string "at"
    space

    maybeT <- optional time

    case maybeT of
        Nothing -> pure $ DateTime date (TimeOfDay 0 0 0 0)
        Just t -> pure $ DateTime date t

-- parser that resets the time of day to the offset
possiblyOffsetTime :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => m DateTime -> m DateTime
possiblyOffsetTime dtm = do
  DateTime date t <- dtm
  t' <- optional $ do
    space
    string "at"
    space
    time
  pure $ DateTime date (fromMaybe t t')

pAbsDate :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => Int -> m Date
pAbsDate year = choice $ map (try . dateInFormat year)
    [euroNumDate, americanDate, strDate, writtenDate, dashDate,
     dotDateMonth, dashDateMonth, slashDateMonth, spaceDate, spaceDateMD]

intervalToPeriod :: DateInterval -> Period
intervalToPeriod (Days ds)   = mempty { periodDays   = ds}
intervalToPeriod (Weeks ws)  = mempty { periodDays   = ws*7 }
intervalToPeriod (Months ms) = mempty { periodMonths = ms }
intervalToPeriod (Years ys)  = mempty { periodYears  = ys }

-- | Add date interval to DateTime
addInterval :: DateTime -> DateInterval -> DateTime
addInterval dt@DateTime {dtDate = date} interval =
  dt { dtDate = date `dateAddPeriod` intervalToPeriod interval }

-- | Negate DateInterval value: Days 3 -> Days (-3).
negateInterval :: DateInterval -> DateInterval
negateInterval (Days n)   = Days (negate n)
negateInterval (Weeks n)  = Weeks (negate n)
negateInterval (Months n) = Months (negate n)
negateInterval (Years n)  = Years (negate n)

-- | Subtract DateInterval from DateTime.
minusInterval :: DateTime -> DateInterval -> DateTime
minusInterval date int = date `addInterval` negateInterval int

maybePlural :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => Tokens s -> m (Tokens s)
maybePlural str = do
  r <- string str
  optional $ char 's'
  return r

pDateIntervalType :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => m (Int -> DateInterval)
pDateIntervalType = do
  s <- choice $ map (\w -> maybePlural (fromString w) *> pure w) ["day", "week", "month", "year"]
  case toLower (head s) of
    'd' -> return Days
    'w' -> return Weeks
    'm' -> return Months
    'y' -> return Years
    _ -> fail $ "Unknown date interval type: " ++ s

pDateInterval :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => m DateInterval
pDateInterval = do
  maybeN <- readMaybe <$> some digitChar

  case maybeN of
    Nothing -> fail "Noperino."
    Just n -> do
      space
      tp <- pDateIntervalType
      pure $ tp n

pRelTime :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => Config -> m DateTime
pRelTime c = do
  offs <- pRelTimeEitherDir
  pure $ (c ^. now) `timeAdd` offs

pRelDate :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => Config -> m DateTime
pRelDate c = do
  offs <- try futureDate
     <|> try pastDate
     <|> try today
     <|> try tomorrow
     <|> yesterday
  return $ (c^.now) `addInterval` offs

lastDate :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => Config -> m DateTime
lastDate c = do
    string "last"
    space
    try byweek <|> try bymonth <|> byyear
  where
    startOfWeekDay' = c^.startOfWeekDay
    now' = c^.now
    byweek = do
      wd <- try (string "week" >> return startOfWeekDay') <|> pWeekDay
      let lastWeekStart = getStartOfThisWeek c `minusInterval` Weeks 1
      return $ lastWeekStart `addInterval` weekdayToInterval c wd

    bymonth = do
      string "month"
      let lastMonth = now' `minusInterval` Months 1
      return $ lastMonth { dtDate = (dtDate lastMonth) { dateDay = 1 } }

    byyear = do
      string "year"
      let lastYear = now' `minusInterval` Years 1
      return $ lastYear { dtDate = (dtDate lastYear) { dateMonth = January, dateDay = 1 } }

nextDate :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => Config -> m DateTime
nextDate c = do
    string "next"
    space
    try byweek <|> try bymonth <|> byyear
  where
    startOfWeekDay' = c^.startOfWeekDay
    now' = c^.now
    byweek = do
      wd <- try (string "week" >> return startOfWeekDay') <|> pWeekDay
      let nextWeekStart = getStartOfNextWeek c
      return $ nextWeekStart `addInterval` weekdayToInterval c wd

    bymonth = do
      string "month"
      let nextMonth = now' `addInterval` Months 1
      return nextMonth { dtDate = (dtDate nextMonth) { dateDay = 1 } }

    byyear = do
      string "year"
      let nextYear = now' `addInterval` Years 1
      return nextYear { dtDate = (dtDate nextYear) { dateMonth = January, dateDay = 1 } }

pWeekDay :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => m WeekDay
pWeekDay = do
  w <- some (oneOf @[] "mondaytueswnhrfi")
  case uniqFuzzyMatch w :: Either [WeekDay] WeekDay of
    Left ds -> fail $ if null ds
                         then "unknown weekday: " ++ w
                         else "ambiguous weekday '" ++ w ++ "' could mean: " ++ intercalate " or " (map show ds)
    Right d -> return d

futureDate :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => m DateInterval
futureDate = do
  string "in "
  maybeN <- readMaybe <$> some digitChar

  case maybeN of
    Nothing -> fail "Noperino."
    Just n -> do
      space
      tp <- pDateIntervalType
      pure $ tp n

pastDate :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => m DateInterval
pastDate = do
  maybeN <- readMaybe <$> some digitChar

  case maybeN of
    Nothing -> fail "Could not parse digit."
    Just n -> do
      space
      tp <- pDateIntervalType
      string " ago"
      pure $ tp $ negate n

today :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => m DateInterval
today = do
  string "today" <|> string "now"
  return $ Days 0

tomorrow :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => m DateInterval
tomorrow = do
  string "tomorrow"
  return $ Days 1

yesterday :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => m DateInterval
yesterday = do
  string "yesterday"
  return $ Days (-1)

pByWeek :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => Config -> m DateTime
pByWeek c =
  try (lastDate c) <|> nextDate c

-- | Parsec parser for DateTime.
pDateTime :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s))
          => Config
          -> m DateTime
pDateTime c =
      try (pRelTime c)
  <|> try (possiblyOffsetTime $ pRelDate c)
  <|> try (possiblyOffsetTime $ pByWeek c)
  <|> try (pAbsDateTime (dateYear (timeGetDate (c^.now))))

-- | Parsec parser for Date only.
pDate :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s))
      => Config
      -> m Date
pDate c =
      try (timeGetDate <$> pRelDate c)
  <|> try (timeGetDate <$> pByWeek c)
  <|> try (pAbsDate $ dateYear (timeGetDate (c^.now)))

-- | Parse date/time
parseDate :: (Ord e, Stream s, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s))
          => Config
          -> s
          -> Either (ParseErrorBundle s e) Date
parseDate c = runParser (pDate c) ""

-- | Parse date/time
parseDateTime :: (Ord e, Stream s, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s))
              => Config
              -> s
              -> Either (ParseErrorBundle s e) DateTime
parseDateTime c = runParser (pDateTime c) ""

-- | Same as extractDatesY, but will get the current year from the system, so you don't have to provide it.
extractDates :: String -> IO [Date]
extractDates str = extractDatesConfig <$> defaultConfigIO <*> pure str

extractDatesConfig :: Config -> String -> [Date]
extractDatesConfig config str =
    case parse @String (extract (pDate config)) "" str of
        Left err -> []
        Right dates -> dates

-- | Extract dates from a string, with the first argument being the current year (used for things like "Jan 18").
--
-- >>> extractDatesY 2018 "The party will be on 6/9"
-- [Date 2018 June 9]
extractDatesY :: Int -> String -> [Date]
extractDatesY y str =
    case parse @String (extract (pAbsDate y)) "" str of
        Left err -> error $ show err
        Right dates -> dates

extractDateTimes :: String -> IO [DateTime]
extractDateTimes str = extractDateTimesConfig <$> defaultConfigIO <*> pure str

-- | Extract dates with optional times from a string, with the first argument being the current year (used for things like "Jan 18").
-- If no time is specified, will return time at midnight.
--
-- Note: This function **WILL NOT** parse relative dates like "2 weeks ago." For that, you must use `extractDateTimesConfig` or `extractDateTimes`, because the parser needs to know the exact date.
--
-- >>> extractDateTimesY 2018 "The talk starts at 12.09.12 8:00 AM"
-- [DateTime {dtDate = Date {dateYear = 2012, dateMonth = September, dateDay = 12}, dtTime = TimeOfDay {todHour = 8h, todMin = 0m, todSec = 0s, todNSec = 0ns}}]
--
-- >>> extractDateTimesY 2018 "The party will be on 6/9"
-- [DateTime {dtDate = Date {dateYear = 2018, dateMonth = June, dateDay = 9}, dtTime = TimeOfDay {todHour = 0h, todMin = 0m, todSec = 0s, todNSec = 0ns}}]
extractDateTimesY :: Int -> String -> [DateTime]
extractDateTimesY y str =
    case parse @String (extract (pAbsDateTime y)) "" str of
        Left err -> []
        Right dates -> dates

extractDateTimesConfig :: Config -> String -> [DateTime]
extractDateTimesConfig config str =
    case parse @String (extract (pDateTime config)) "" str of
        Left err -> []
        Right dates -> dates

extract :: (MonadFail m, MonadParsec e s m, Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => m a -> m [a]
extract parser = catMaybes <$> Text.Megaparsec.manyTill (try (Just <$> loop) <|> (anySingle >> pure Nothing)) eof
    where
        loop = try parser <|> do
            anySingle
            notFollowedBy eof
            loop

