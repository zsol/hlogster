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

type MetricFun = [String] -> Float
data Metric = Metric {
  name :: String,
  apply :: MetricFun
  }

getCategoryAndEvent :: String -> (String, String)
getCategoryAndEvent line = let tokens = words line in (tokens !! 3, splitOn ":" (tokens !! 5) !! 1)

countEvents :: String -> String -> MetricFun
countEvents category eventId = fromIntegral . length . filter (((category, eventId) ==) . getCategoryAndEvent)

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
