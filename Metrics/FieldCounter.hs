module Metrics.FieldCounter where

import qualified Data.ByteString.Char8           as SB
import qualified Data.ByteString.Lazy.Char8      as B
import Metrics.Common
import Parsers (getFields)
import Data.Either

countFields :: [(Int, SB.ByteString)] -> String -> B.ByteString -> MetricState
countFields spec nameString input = CounterMetricState nameString $ case match input of
  Right _ -> 1
  Left _  -> 0
  where
    match line = do
      fields <- getFields (map fst spec) line
      case fields == map snd spec of
        True -> return fields
        False -> Left ""
