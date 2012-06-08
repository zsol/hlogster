module Metric1 (count_events, Metric, MetricFun) where

import Data.List.Split (splitOn)
type MetricFun = [[String]] -> Int
type Metric = (String, MetricFun)

match_cat tokens cat = tokens !! 3 == cat
fn_name tokens = (splitOn ":" (tokens !! 5)) !! 1
match_fn_name tokens fn = fn_name tokens == fn
match_cat_and_fn cat fn tokens = (match_cat tokens cat) && (match_fn_name tokens fn)

count_events :: String -> String -> [[String]] -> Int

count_events cat fn lines =   (length (filter (match_cat_and_fn cat fn) lines))
