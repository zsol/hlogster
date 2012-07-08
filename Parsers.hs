{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Parsers
       (getCategoryAndEvent)
       where
import Text.Parsec hiding (endBy)
import Data.List.Split (endBy)
import Data.Char (isSpace)

nonspace :: Stream s m Char => ParsecT s u m Char
nonspace = satisfy $ not . isSpace

field :: Stream s m Char => ParsecT s u m String
field = do
  ret <- many1 nonspace
  spaces
  return ret

field_ :: Stream s m Char => ParsecT s u m ()
field_ = (skipMany1 space) <|> (anyChar >> field_)

categoryAndEvent :: Stream s m Char => ParsecT s u m (String, String)
categoryAndEvent = do
  count 3 field_
  category <- field
  count 2 field_
  event <- field
  return (category, last $ endBy ":" event)

--getCategoryAndEvent :: Stream s Data.Functor.Identity.Identity Char => s -> Either ParseError (String, String)
getCategoryAndEvent = parse categoryAndEvent ""
