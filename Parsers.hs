{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Parsers
       (getCategoryAndEvent, getFields, getFieldsWithTime)
       where
import Data.Attoparsec.Lazy
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as SB
import Data.List (sort)
import Data.Time.Clock
import Data.Time.Format
import System.Locale

datetime :: Parser UTCTime
datetime = do
  date <- field
  time <- field
  case parseTime defaultTimeLocale "%F %T,%q" ((SB.unpack date) ++ " " ++ (SB.unpack time)) of
    Just a -> return a
    _      -> fail "Invalid time format"

spaces :: Parser ()
spaces = skipWhile $ inClass " \t"

field_ :: Parser ()
field_ = (skipWhile $ notInClass " \t") >> spaces

field :: Parser SB.ByteString
field = do
  ret <- takeWhile1 $ notInClass " \t"
  spaces
  return ret

fields :: [Int] -> Parser [SB.ByteString]
fields [] = return []
fields (n:ns) = do
  count (n-1) field_
  ret <- field
  let newns = map (flip (-) n) ns
  rest <- fields newns
  return (ret:rest)

fieldsWithTime :: [Int] -> Parser (UTCTime, [SB.ByteString])
fieldsWithTime l = do
  time <- datetime
  rest <- fields $ map (flip (-) 2) l
  return (time, rest)

getFields :: [Int] -> B.ByteString -> Either String [SB.ByteString]
getFields numbers = eitherResult . parse (fields $ sort numbers)

getFieldsWithTime :: [Int] -> B.ByteString -> Either String (UTCTime, [SB.ByteString])
getFieldsWithTime numbers = eitherResult . parse (fieldsWithTime $ sort numbers)

categoryAndEvent :: Parser (SB.ByteString, SB.ByteString)
categoryAndEvent = do
  count 3 field_
  category <- field
  count 2 field_
  event <- field
  return (category, last $ SB.split  ':' event)

getCategoryAndEvent :: B.ByteString -> Either String (SB.ByteString, SB.ByteString)
getCategoryAndEvent = eitherResult . parse categoryAndEvent
