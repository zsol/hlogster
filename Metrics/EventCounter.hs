module Metrics.EventCounter where

import ConfigBase
import Metrics.Common
import qualified Data.ByteString.Char8           as SB
import qualified Data.ByteString.Lazy.Char8      as B
import Parsers (getCategoryAndEvent)

makeEventCounter :: Config -> Either String (Metric MetricState)
makeEventCounter EventCounter {name = name, event = event, category = category} = Right $
  countEvents (SB.pack category) (SB.pack event) name
makeEventCounter _ = Left "eventcounter config barf."

countEvents :: SB.ByteString -> SB.ByteString -> String -> B.ByteString -> MetricState
countEvents category eventId nameString input = CounterMetricState nameString value
  where
    value = case getCategoryAndEvent input of
      Right catEv -> if catEv == (category, eventId) then 1 else 0
      _           -> 0

