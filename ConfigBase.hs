module ConfigBase where

import Text.Regex.PCRE.ByteString (Regex)
import RegexCompiler (compileRegex)

compileRegex = RegexCompiler.compileRegex

data Config =
  EventCounter {name :: String, category :: String, event :: String} |
  FieldCounter {name :: String, fields :: [FieldSpec]} |
  RegexCounter {name :: String, regex :: Regex} |
  RegexTiming {name :: String, regex :: Regex, durationGroup :: Int, nameSuffixes :: [Int]} |
  JsonMetric {name :: String, matchFields :: [FieldSpec], jsonFieldIndex :: Int, jsonKeys :: [String]}

data FieldSpec = Field { index :: Int, match :: String }

