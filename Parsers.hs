{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Parsers
       (getCategoryAndEvent)
       where
import Data.Attoparsec
import qualified Data.ByteString.Char8 as B

spaces :: Parser ()
spaces = skipWhile $ inClass " \t"

field_ :: Parser ()
field_ = (skipWhile $ notInClass " \t") >> spaces

field :: Parser B.ByteString
field = do
  ret <- takeWhile1 $ notInClass " \t"
  spaces
  return ret

categoryAndEvent :: Parser (B.ByteString, B.ByteString)
categoryAndEvent = do
  count 3 field_
  category <- field
  count 2 field_
  event <- field
  return (category, last $ B.split  ':' event)

getCategoryAndEvent :: B.ByteString -> Either String (B.ByteString, B.ByteString)
getCategoryAndEvent = eitherResult . parse categoryAndEvent
