{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Parsers
       (getCategoryAndEvent)
       where
import Data.Attoparsec.Lazy
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as SB

spaces :: Parser ()
spaces = skipWhile $ inClass " \t"

field_ :: Parser ()
field_ = (skipWhile $ notInClass " \t") >> spaces

field :: Parser SB.ByteString
field = do
  ret <- takeWhile1 $ notInClass " \t"
  spaces
  return ret

categoryAndEvent :: Parser (SB.ByteString, SB.ByteString)
categoryAndEvent = do
  count 3 field_
  category <- field
  count 2 field_
  event <- field
  return (category, last $ SB.split  ':' event)

getCategoryAndEvent :: B.ByteString -> Either String (SB.ByteString, SB.ByteString)
getCategoryAndEvent = eitherResult . parse categoryAndEvent
