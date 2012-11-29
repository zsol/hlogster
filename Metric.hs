module Metric (
  Metric,
  IMetricState(combine, toResults),
  MetricState,
  Results,
  Timestamp,
  getResults,
  makeMetric
  ) where

import qualified ConfigBase                      as Conf
import           Control.Parallel.Strategies     (parMap, rdeepseq)
import           Data.Array                      as A
import qualified Data.ByteString.Char8           as SB
import qualified Data.ByteString.Lazy.Char8      as B
import           Data.Function                   (on)
import           Data.List
import qualified Data.Map                        as M
import           Parsers
import           Text.Regex.Base.RegexLike       (MatchText, matchAllText,
                                                  matchCount)
import           Text.Regex.PCRE.ByteString.Lazy

-- this is implemented properly in bytestring > 0.10
import           Control.DeepSeq
import           Data.ByteString.Lazy.Internal
instance NFData ByteString where
  rnf Empty       = ()
  rnf (Chunk _ b) = rnf b


type (Metric state) = [B.ByteString] -> state
type Timestamp = String
type Results = [(String, String, Timestamp)]

class IMetricState a
  where
    combine :: a -> a -> a
    toResults :: Timestamp -> a -> Results
    toResultsNow :: a -> Results

    toResultsNow = toResults ""

data TimingMetricState = TimingMetricState {
    min' :: Float,
    max' :: Float,
    avg' :: Float,
    num' :: Float
    }

data MetricState =
  CounterMetricState String Float |
  Timings (M.Map String TimingMetricState)

combineTimingState :: TimingMetricState -> TimingMetricState -> TimingMetricState
combineTimingState (TimingMetricState {min' = amin, max' = amax, avg' = aavg, num' = anum}) (TimingMetricState {min' = bmin, max' = bmax, avg' = bavg, num' = bnum}) = TimingMetricState {
      min' = min amin bmin,
      max' = max amax bmax,
      avg' = ((aavg * anum) + (bavg * bnum)) / (anum + bnum),
      num' = anum + bnum
      }


instance IMetricState MetricState
  where
    combine (CounterMetricState name' a) (CounterMetricState _ b) = CounterMetricState name' (a+b)
    combine (Timings a) (Timings b) = Timings $ M.unionWith combineTimingState a b
    combine _ _ = undefined

    toResults timestamp (CounterMetricState name' a)
      | a == 0 = []
      | otherwise = [(name', show a, timestamp)]
    toResults timestamp (Timings a) = concat [[ (name ++ "." ++ x, show $ y state, timestamp) | (x, y) <- zip ["min", "max", "avg", "count"] [min', max', avg', num']] | (name, state) <- M.toList a] -- omg so ugly

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

countFields :: [(Int, SB.ByteString)] -> String -> [B.ByteString] -> MetricState
countFields spec nameString input = CounterMetricState nameString (fromIntegral $ length $ matchingFields input)
  where
    matchingFields [] = []
    matchingFields (l:ls)
      | map snd spec == selectedFields l = () : matchingFields ls
      | otherwise                        = matchingFields ls
    selectedFields line = case getFields (map fst spec) line of
      Right a -> a
      Left _ -> []

countRegexen :: Regex -> String -> [B.ByteString] -> MetricState
countRegexen regex nameString input = CounterMetricState nameString (fromIntegral $ sum $ map (matchCount regex) input)

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

average :: Fractional a => [a] -> a
average a = sum a / fromIntegral (length a)

select :: Ix i => [i] -> Array i a -> [a]
select [] _ = []
select (x:xs) arr = arr A.! x : select xs arr

makeMetric :: Conf.Config -> Metric MetricState
makeMetric Conf.EventCounter {Conf.name = name', Conf.event = event, Conf.category = category} =
  countEvents (SB.pack category) (SB.pack event) name'
makeMetric Conf.FieldCounter {Conf.name = name', Conf.fields = fields} =
  countFields (map (\x -> (Conf.index x, SB.pack $ Conf.match x)) fields) name'
makeMetric Conf.RegexCounter {Conf.name = name', Conf.regex = regex} =
  countRegexen regex name'
makeMetric Conf.RegexTiming {Conf.name = name', Conf.regex = regex, Conf.durationGroup = durationGroup, Conf.nameSuffixes = nameSuffixes} =
  timingRegex regex name' durationGroup nameSuffixes

getResults :: IMetricState state => Metric state -> [B.ByteString] -> Results
getResults metric input = toResultsNow $ metric input
