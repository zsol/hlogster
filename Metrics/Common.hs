{-# LANGUAGE ExistentialQuantification, Rank2Types, TemplateHaskell, CPP #-}
module Metrics.Common where

import qualified Data.ByteString.Lazy.Char8      as B
import qualified Data.Map                        as M
import Data.Ratio
import           Control.DeepSeq

-- this hackery with macros is needed to make ghci load this file
#ifdef MIN_VERSION_bytestring
#if MIN_VERSION_bytestring(0,10,0)
-- this is implemented properly in bytestring > 0.10
{-
#endif
#endif
import           Data.ByteString.Lazy.Internal
import qualified Data.ByteString.Char8 as SB
instance NFData SB.ByteString
instance NFData ByteString where
  rnf Empty       = ()
  rnf (Chunk _ b) = rnf b
#ifdef MIN_VERSION_bytestring
#if MIN_VERSION_bytestring(0,10,0)
-}
#endif
#endif

instance NFData MetricState

type Metric a = B.ByteString -> a
type Timestamp = String
type Results = [(String, String, Timestamp)]

class IMetricState a
  where
    combine :: a -> a -> a
    toResults :: Timestamp -> a -> Results
    toResultsNow :: a -> Results

    toResultsNow = toResults ""

data MetricState
  = CounterMetricState String Float
  | Timings (M.Map String TimingMetricState)
  | Timings2 (M.Map String [Double])
  | JsonMetricState (M.Map String [String])

data TimingMetricState = TimingMetricState {
    min' :: Double,
    max' :: Double,
    avg' :: Double,
    num' :: Double
    }

instance IMetricState MetricState
  where
    combine (CounterMetricState name' a) (CounterMetricState _ b) = CounterMetricState name' (a+b)
    combine (Timings a) (Timings b) = Timings $ M.unionWith combineTimingState a b
    combine (Timings2 a) (Timings2 b) = Timings2 $ M.unionWith (++) a b
    combine (JsonMetricState a) (JsonMetricState b) = JsonMetricState $ M.union a b
    
    toResults timestamp (Timings2 a) = concat [map (\(x, y) -> (key ++ "." ++ x, show y, timestamp)) (calculateMetrics values) | (key, values) <- M.toList a]
    toResults timestamp (Timings a) = concat [[ (name ++ "." ++ x, show $ y state, timestamp) | (x, y) <- zip ["min", "max", "avg", "count"] [min', max', avg', num']] | (name, state) <- M.toList a] -- omg so ugly
    toResults timestamp (CounterMetricState name' a)
      | a == 0 = []
      | otherwise = [(name', show a, timestamp)]
    toResults timestamp (JsonMetricState a) = concat [ zip3 (repeat key) values (repeat timestamp) | (key, values) <- M.toList a]

combineTimingState :: TimingMetricState -> TimingMetricState -> TimingMetricState
combineTimingState (TimingMetricState {min' = amin, max' = amax, avg' = aavg, num' = anum}) (TimingMetricState {min' = bmin, max' = bmax, avg' = bavg, num' = bnum}) = TimingMetricState {
      min' = min amin bmin,
      max' = max amax bmax,
      avg' = ((aavg * anum) + (bavg * bnum)) / (anum + bnum),
      num' = anum + bnum
      }

calculateMetrics :: [Double] -> [(String, Double)]
calculateMetrics floats = [(key, metric floats) | (key, metric) <- [("min", minimum), ("max", maximum), ("avg", average), ("count", fromIntegral . length), ("90p", percentile 90), ("99p", percentile 99), ("99_9p", quantile 1000 999)]]

quantile :: Integral a => a -> a -> [b] -> b
quantile q k xs = xs !! (ind - 1)
  where
    ind = floor $ (k % q) * (fromIntegral $ length xs) + (1 % 2)

percentile :: Integral a => a -> [b] -> b
percentile = quantile 100

average :: Fractional a => [a] -> a
average a = sum a / fromIntegral (length a)

getResults :: IMetricState a => Metric a -> B.ByteString -> Results
getResults metric input = toResultsNow $ metric input

-- makeEventCounter x = undefined

-- declareMetric :: [String] -> Q [Dec]
-- declareMetric nameStrs@(nameStr:_) = do
--   names <- mapM newName nameStrs
--   let factoryNames = map (mkName . ("make" ++)) nameStrs
--   mkMetricTpl <- [d|
--                   makeMetric2 :: Config -> Metric
--                   makeMetric2 x = case makeEventCounter x of 
--                                     Right y -> y
--                                     _ -> undefined
--                  |]
--   error $ show mkMetricTpl
--   retTypes <- mapM (getFnRetType . ("make" ++)) nameStrs
--   metricName <- newName "MetricType"
--   dec <- dataD (cxt []) metricName [] (map (flip normalC []) names) []
--   return [dec]
--   where
--     getFnRetType fnName = do
--       info <- reify $ mkName fnName
--       case info of
--         VarI _ (AppT (AppT ArrowT (ConT configName)) (AppT (AppT (ConT eitherName) (ConT stringName)) (ConT state))) _ _ -> error $ "Yay " ++ show state
--         _ -> error $ "Boo" ++ show info
      
