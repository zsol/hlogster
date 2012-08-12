{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module RegexCompiler (compileRegex, re)
where

import Data.ByteString.Lazy.Char8
import Text.Regex.TDFA.ByteString.Lazy
import Text.Regex.Base.RegexLike
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

fromRight :: Either t t1 -> t1
fromRight (Right a) = a
fromRight _ = error "fromRight called with Left"

re :: QuasiQuoter
re = QuasiQuoter { quoteExp = compileRegex, quotePat = undefined, quoteType = undefined, quoteDec = undefined }

compileRegex :: String -> Q Exp
compileRegex regex = 
  case compile defaultCompOpt defaultExecOpt (pack regex) of
    Left a -> fail a
    Right _ -> [| fromRight $ compile defaultCompOpt defaultExecOpt (pack regex) |]
