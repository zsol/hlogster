module Metric (
  Metric(Metric),
  MetricFun,
  name,
  apply,
  countEvents
  ) where

import Data.List.Split (splitOn)

type MetricFun = [String] -> Float
data Metric = Metric {
  name :: String,
  apply :: MetricFun
  }

getCategoryAndEvent :: String -> (String, String)
getCategoryAndEvent line = let tokens = words line in (tokens !! 3, splitOn ":" (tokens !! 5) !! 1)

countEvents :: String -> String -> MetricFun
countEvents category eventId = fromIntegral . length . filter (((category, eventId) ==) . getCategoryAndEvent)
    