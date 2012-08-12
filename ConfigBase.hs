module ConfigBase where

import Text.Regex.TDFA.ByteString.Lazy (Regex)
import RegexCompiler (compileRegex)

compileRegex = RegexCompiler.compileRegex

data Config =
  EventCounter {name :: String, category :: String, event :: String} |
  FieldCounter {name :: String, fields :: [FieldSpec]} |
  RegexCounter {name :: String, regex :: Regex}

data FieldSpec = Field { index :: Int, match :: String }

