module Metric (
  Metric(Metric),
  MetricFun,
  name,
  apply,
  countEvents,
  parseConfig
  ) where

import Data.List.Split (splitOn)
import Control.Monad ((>=>))
import Text.JSON
import Parsers
import Data.Maybe

type MetricFun = [String] -> Float
data Metric = Metric {
  name :: String,
  apply :: MetricFun
  }  

countEvents :: String -> String -> MetricFun
countEvents category eventId = fromIntegral . length . filter (((category, eventId) ==) . categoryAndEvent)
  where
    categoryAndEvent line = case getCategoryAndEvent line of
      Right a -> a
      Left _ -> ("", "") -- TODO

makeEventCounter :: JSObject JSValue -> Result Metric
makeEventCounter obj = let (!) = flip valFromObj in do
  nameString <- obj ! "name"
  category <- obj ! "category"
  event <- obj ! "event"
  return $ Metric {name = nameString, apply = countEvents category event}

makeMetric :: JSObject JSValue -> Result Metric
makeMetric obj = let (!) = flip valFromObj in do
  typeString <- obj ! "type"
  case typeString of
    "eventCounter" -> makeEventCounter obj
    _ -> Error "unknown metric type"

parseConfig :: String -> Either String [Metric]
parseConfig = toEither . (decode >=> mapM makeMetric)
  where
    toEither (Ok a)    = Right a
    toEither (Error a) = Left a
