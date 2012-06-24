module Metric1 (countEvents, Metric, MetricFun) where

import Data.List.Split (splitOn)
type MetricFun = [[String]] -> Int
type Metric = (String, MetricFun)

matchCat :: Eq a => [a] -> a -> Bool
matchCat tokens cat = tokens !! 3 == cat
fnName :: [String] -> String
fnName tokens = splitOn ":" (tokens !! 5) !! 1
matchFnName :: [String] -> String -> Bool
matchFnName tokens fn = fnName tokens == fn
matchCatAndFn :: String -> String -> [String] -> Bool
matchCatAndFn cat fn tokens = matchCat tokens cat && (matchFnName tokens fn)

countEvents :: String -> String -> [[String]] -> Int
countEvents cat fn = length . filter (matchCatAndFn cat fn)
