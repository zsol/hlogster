{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module RegexCompiler (compileRegex, re)
where

import Data.ByteString.Lazy.Char8
import Text.Regex.PCRE.ByteString.Lazy
import Text.Regex.Base.RegexLike
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import System.IO.Unsafe (unsafePerformIO)

fromRight :: Either t t1 -> t1
fromRight (Right a) = a
fromRight _ = error "fromRight called with Left"

re :: QuasiQuoter
re = QuasiQuoter { quoteExp = compileRegex, quotePat = undefined, quoteType = undefined, quoteDec = undefined }

compileRegex :: String -> Q Exp
compileRegex regex = 
  case unsafePerformIO (compile defaultCompOpt defaultExecOpt (pack regex)) of
    Left a -> fail $ snd a
    Right _ -> [| fromRight $ unsafePerformIO (compile defaultCompOpt defaultExecOpt (pack regex)) |]
