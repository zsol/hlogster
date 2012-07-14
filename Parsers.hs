{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Parsers
       (getCategoryAndEvent, getFields, getFieldsWithTime, getDatetime)
       where
import Data.Attoparsec.Lazy
import qualified Data.Attoparsec.Char8 as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as SB
import Data.List (sort)

datetime :: Parser Integer
datetime = do
  date <- A.decimal `sepBy1` A.char '-'
  A.char ' '
  time <- A.decimal `sepBy1` A.char ':'
  field_
  return $ foldr (\(x,y) s -> s + x * y) 0 (zip [366*24*60*60, 31*24*60*60, 24*60*60, 60*60, 60, 1] (date ++ time))

getDatetime = eitherResult . parse datetime

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

fieldsWithTime :: [Int] -> Parser (Integer, [SB.ByteString])
fieldsWithTime l = do
  time <- datetime
  rest <- fields $ map (flip (-) 2) l
  return (time, rest)

getFields :: [Int] -> B.ByteString -> Either String [SB.ByteString]
getFields numbers = eitherResult . parse (fields $ sort numbers)

getFieldsWithTime :: [Int] -> B.ByteString -> Either String (Integer, [SB.ByteString])
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
