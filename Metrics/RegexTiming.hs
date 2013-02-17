module Metrics.RegexTiming(timingRegex, timingRegex2) where

import Metrics.Common
import qualified Data.ByteString.Lazy.Char8      as B
import Text.Regex.Base.RegexLike (matchAllText, MatchText)
import Text.Regex.PCRE.ByteString.Lazy
import qualified Data.Map                        as M
import           Data.Array                      as A
import Data.List (groupBy)
import Data.Function (on)
import Data.Attoparsec.ByteString.Lazy
import Data.Attoparsec.ByteString.Char8 (double)

select :: Ix i => [i] -> Array i a -> [a]
select [] _ = []
select (x:xs) arr = arr A.! x : select xs arr

match :: Regex -> [B.ByteString] -> [MatchText B.ByteString]
match regex input = {-# SCC "matchConcat" #-} concat $ map (matchAllText regex) input

parseDouble :: B.ByteString -> Double
parseDouble input = {-# SCC "parseDouble" #-} case parse double input of
  Done _ r -> r
  Fail _ _ _ -> 0

duration :: Int -> [MatchText B.ByteString] -> [Double]
duration durationGroup matches = {-# SCC "duration" #-} map (parseDouble . fst . (A.! durationGroup)) matches

name :: [Int] -> [MatchText B.ByteString] -> [B.ByteString]
name nameSuffixes matches = {-# SCC "name" #-} map (B.intercalate (B.pack ".") . map fst . select nameSuffixes) matches

pair :: ([b] -> t) -> [(a, b)] -> (a, t)
pair _ [] = error "Internal error in timingRegex: pair applied to empty list"
pair state durs@((name,_):_) = {-# SCC "pair" #-}  (name, state (map snd durs))


durationByName :: [b] -> [B.ByteString] -> [[(B.ByteString, b)]]
durationByName durations names = {-# SCC "byname" #-} case durations of
  [] -> []
  _  -> case names of
    [] -> [zip (repeat B.empty) durations]
    _  -> groupBy ((==) `on` fst) (zip names durations)

timingRegex :: Regex -> String -> Int -> [Int] -> [B.ByteString] -> MetricState
timingRegex regex nameString durationGroup nameSuffixes input = Timings $ M.fromList $ map buildName states
  where
    buildName (suffix, metricStates)
      | B.null suffix = (nameString, metricStates)
      | otherwise     = (nameString ++ "." ++ B.unpack suffix, metricStates)
    matches = match regex input
    durations = duration durationGroup matches
    names = name nameSuffixes matches
    durationsByName = durationByName durations names
    states :: [(B.ByteString, TimingMetricState)]
    states = map (pair state) durationsByName
    state durs = {-# SCC "state1" #-} TimingMetricState {min' = minimum durs, max' = maximum durs,
                                    avg' = average durs, num' = fromIntegral $ length durs}

timingRegex2 :: Regex -> String -> Int -> [Int] -> [B.ByteString] -> MetricState
timingRegex2 regex nameString durationGroup nameSuffixes input = Timings2 $ M.fromList $ map buildName states
  where
    buildName (suffix, metricStates)
      | B.null suffix = (nameString, metricStates)
      | otherwise     = (nameString ++ "." ++ B.unpack suffix, metricStates)
    matches = match regex input
    durations = duration durationGroup matches
    names = name nameSuffixes matches
    durationsByName = durationByName durations names
    states = map (pair id) durationsByName
