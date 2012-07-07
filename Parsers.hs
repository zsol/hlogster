{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Parsers
       (getCategoryAndEvent)
       where
import Text.Parsec

field :: Stream s m Char => ParsecT s u m String
field = anyChar `manyTill` space

categoryAndEvent :: Stream s m Char => ParsecT s u m (String, String)
categoryAndEvent = do
  count 3 field
  category <- field
  anyChar `manyTill` (char ':')
  event <- field
  return (category, event)

--getCategoryAndEvent :: Stream s Data.Functor.Identity.Identity Char => s -> Either ParseError (String, String)
getCategoryAndEvent = parse categoryAndEvent ""
