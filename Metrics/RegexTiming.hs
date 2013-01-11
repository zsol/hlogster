{-# LANGUAGE CPP #-}
module Metrics.RegexTiming where

import Metrics.Common
import qualified Data.ByteString.Lazy.Char8      as B
import Text.Regex.Base.RegexLike (matchAllText, MatchText)
import Text.Regex.PCRE.ByteString.Lazy
import qualified Data.Map                        as M
import Control.Parallel.Strategies     (parMap, rdeepseq)
import           Data.Array                      as A
import Data.List (groupBy)
import Data.Function (on)

-- this hackery with macros is needed to make ghci load this file
#ifdef MIN_VERSION_bytestring
#if MIN_VERSION_bytestring(0,10,0)
-- this is implemented properly in bytestring > 0.10
{-
#endif
#endif
import           Control.DeepSeq
import           Data.ByteString.Lazy.Internal
instance NFData ByteString where
  rnf Empty       = ()
  rnf (Chunk _ b) = rnf b
#ifdef MIN_VERSION_bytestring
#if MIN_VERSION_bytestring(0,10,0)
-}
#endif
#endif



select :: Ix i => [i] -> Array i a -> [a]
select [] _ = []
select (x:xs) arr = arr A.! x : select xs arr

timingRegex :: Regex -> String -> Int -> [Int] -> [B.ByteString] -> MetricState
timingRegex regex nameString durationGroup nameSuffixes input = Timings $ M.fromList $ map buildName states
  where
    buildName (suffix, metricStates)
      | B.null suffix = (nameString, metricStates)
      | otherwise     = (nameString ++ "." ++ B.unpack suffix, metricStates)
    matches :: [MatchText B.ByteString]
    matches = concatMap (matchAllText regex) input
    durations = parMap rdeepseq (read . B.unpack . fst . (A.! durationGroup)) matches :: [Float]
    names = parMap rdeepseq (B.intercalate (B.pack ".") . map fst . select nameSuffixes) matches
    durationsByName :: [[(B.ByteString, Float)]]
    durationsByName = case durations of
      [] -> []
      _  -> case names of
        [] -> [zip (repeat B.empty) durations]
        _  -> groupBy ((==) `on` fst) (zip names durations)
    states :: [(B.ByteString, TimingMetricState)]
    states = map pair durationsByName
    pair [] = error "Internal error in timingRegex: pair applied to empty list"
    pair durs@((name,_):_) = (name, state (map snd durs))
    state durs = TimingMetricState {min' = minimum durs, max' = maximum durs,
                                    avg' = average durs, num' = fromIntegral $ length durs}

timingRegex2 :: Regex -> String -> Int -> [Int] -> [B.ByteString] -> MetricState
timingRegex2 regex nameString durationGroup nameSuffixes input = Timings2 $ M.fromList $ map buildName states
  where
    buildName (suffix, metricStates)
      | B.null suffix = (nameString, metricStates)
      | otherwise     = (nameString ++ "." ++ B.unpack suffix, metricStates)
    matches :: [MatchText B.ByteString]
    matches = concatMap (matchAllText regex) input
    durations = parMap rdeepseq (read . B.unpack . fst . (A.! durationGroup)) matches :: [Float]
    names = parMap rdeepseq (B.intercalate (B.pack ".") . map fst . select nameSuffixes) matches
    durationsByName :: [[(B.ByteString, Float)]]
    durationsByName = case durations of
      [] -> []
      _  -> case names of
        [] -> [zip (repeat B.empty) durations]
        _  -> groupBy ((==) `on` fst) (zip names durations)
    states = map pair durationsByName
    pair [] = error "Internal error in timingRegex: pair applied to empty list"
    pair durs@((name,_):_) = (name, map snd durs)
