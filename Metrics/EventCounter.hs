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

countEvents :: SB.ByteString -> SB.ByteString -> String -> [B.ByteString] -> MetricState
countEvents category eventId nameString input = CounterMetricState nameString (fromIntegral $ length $ events input)
  where
    events [] = []
    events (l:ls)
      | (category, eventId) == categoryAndEvent l = () : events ls
      | otherwise                                 = events ls
    categoryAndEvent line = case getCategoryAndEvent line of
      Right a -> a
      Left _ -> (SB.pack "", SB.pack "") -- TODO

