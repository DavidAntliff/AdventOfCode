module Record where
import Text.ParserCombinators.Parsec
import Control.Monad (void)

data DateTime = DateTime { year :: Integer
                         , month :: Integer
                         , day :: Integer
                         , hour :: Integer
                         , minute :: Integer
                         } deriving (Show, Eq, Ord)

data Event = Begin Integer | Sleep | Wake deriving (Show, Eq, Ord)

data Record = Record { datetime :: DateTime
                     , event :: Event
                     } deriving (Show, Eq, Ord)


-- http://jakewheat.github.io/intro_to_parsing/#_lexeme
-- whitespace :: Parser ()
-- whitespace = void $ many $ oneOf " \n\t"

-- lexeme :: Parser a -> Parser a
-- lexeme p = do
--            x <- p
--            whitespace
--            return x

datetimeParser :: Parser DateTime
datetimeParser = do
  year <- count 4 digit
  void $ char '-'
  month <- count 2 digit
  void $ char '-'
  day <- count 2 digit
  void $ char ' '
  hour <- count 2 digit
  void $ char ':'
  minute <- count 2 digit
  return (DateTime (read year) (read month) (read day) (read hour) (read minute))


beginParser :: Parser Event
beginParser = do
  void $ string "Guard #"
  id <- many1 digit
  void $ string " begins shift"
  return (Begin (read id))


sleepParser :: Parser Event
sleepParser = do
  void $ string "falls asleep"
  return (Sleep)


wakeParser :: Parser Event
wakeParser = do
  void $ string "wakes up"
  return (Wake)


recordParser :: Parser Record
recordParser = do
  void $ char '['
  datetime <- datetimeParser
  void $ string "] "
  event <- beginParser <|> wakeParser <|> sleepParser
  return (Record datetime event)


parseRecord :: String -> Record
parseRecord str = case parse recordParser "" str of
  Left err -> error $ "parse error at " ++ (show err)
  Right val -> val

