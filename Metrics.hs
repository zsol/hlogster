{-# LANGUAGE RankNTypes, TemplateHaskell #-}
module Metrics where

import Metrics.Common
import Metrics.EventCounter
import Metrics.FieldCounter
import Metrics.RegexCounter
import Metrics.RegexTiming
import Metrics.JsonMetrics
import ConfigBase
import qualified Data.ByteString.Char8           as SB

makeMetric :: Config -> Metric MetricState
makeMetric EventCounter {name = name, event = event, category = category} = {-# SCC "countEvents" #-}
  countEvents (SB.pack category) (SB.pack event) name
makeMetric FieldCounter {name = name, fields = fields} ={-# SCC "countFields" #-}
  countFields (map (\x -> (index x, SB.pack $ match x)) fields) name
makeMetric RegexCounter {name = name, regex = regex} = {-# SCC "countRegexen" #-}
  countRegexen regex name
makeMetric RegexTiming {name = name, regex = regex, durationGroup = durationGroup, nameSuffixes = nameSuffixes} = {-# SCC "timeRegexen" #-}
  timingRegex2 regex name durationGroup nameSuffixes
makeMetric JsonMetric {name = name, matchFields = fields, jsonFieldIndex = jsonIndex, jsonKeys = jsonKeys} = {-# SCC "jsonMetric" #-}
  jsonMetrics name (map (\x -> (index x, SB.pack $ match x)) fields) jsonIndex jsonKeys

