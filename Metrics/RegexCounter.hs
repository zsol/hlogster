module Metrics.RegexCounter where

import Metrics.Common
import Text.Regex.Base.RegexLike (matchCount)
import           Text.Regex.PCRE.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8      as B

countRegexen :: Regex -> String -> [B.ByteString] -> MetricState
countRegexen regex nameString input = CounterMetricState nameString (fromIntegral $ sum $ map (matchCount regex) input)
