module Metric (
  Metric,
  getResults,
  getResultsBufferedBySecond,
  makeMetric
  ) where

import Parsers
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as SB
import qualified Data.CircularList as C
import Control.Monad.State.Lazy
import Data.CircularList
import Data.Maybe
import Data.Either
import Data.Time.Format
import Data.Time.Clock.POSIX
import System.Locale
import qualified ConfigBase as Conf
import Text.Regex.TDFA.ByteString.Lazy
import Text.Regex.Base.RegexLike
import RegexCompiler

type Metric = [B.ByteString] -> MetricState
type Timestamp = String
type Results = [(String, String, Timestamp)]

class IMetricState a
  where
    combine :: a -> a -> a
    toResults :: Timestamp -> a -> Results
    toResultsNow :: a -> Results

    toResultsNow = toResults ""

data MetricState =
  CounterMetricState String Float |
  TimingMetricState {
    name :: String,
    min' :: Float,
    max' :: Float,
    avg' :: Float,
    num' :: Float
    }

instance IMetricState MetricState
  where
    combine (CounterMetricState name' a) (CounterMetricState _ b) = CounterMetricState name' (a+b) -- todo: check
    combine (TimingMetricState {name = aname, min' = amin, max' = amax, avg' = aavg, num' = anum}) (TimingMetricState {min' = bmin, max' = bmax, avg' = bavg, num' = bnum}) = TimingMetricState {
      name = aname,
      min' = min amin bmin,
      max' = max amax bmax,
      avg' = ((aavg * anum) + (bavg * bnum)) / (anum + bnum),
      num' = anum + bnum
      }
    combine _ _ = undefined

    toResults timestamp (CounterMetricState name' a) = [(name', show a, timestamp)]
    toResults timestamp a = [ (name a ++ "." ++ x, show $ y a, timestamp) | (x, y) <- zip ["min", "max", "avg", "count"] [min', max', avg', num']]

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
countRegexen regex nameString input = CounterMetricState nameString (fromIntegral $ length $ matches input)
  where
    matches = catMaybes . rights . map (execute regex)
      

makeMetric :: Conf.Config -> Metric
makeMetric Conf.EventCounter {Conf.name = name', Conf.event = event, Conf.category = category} =
  countEvents (SB.pack category) (SB.pack event) (name')
makeMetric Conf.FieldCounter {Conf.name = name', Conf.fields = fields} =
  countFields (map (\x -> (Conf.index x, SB.pack $ Conf.match x)) fields) (name')
makeMetric Conf.RegexCounter {Conf.name = name', Conf.regex = regex} =
  countRegexen regex name'

getResults :: Metric -> [B.ByteString] -> Results
getResults metric input = toResultsNow $ metric input

type Time = SB.ByteString
type RingBuffer = CList (Time, MetricState)

getResultsBufferedBySecond :: Int -> ([B.ByteString] -> MetricState) -> [B.ByteString] -> [Results]
getResultsBufferedBySecond maxSize metric input = evalState (process input) C.empty
  where
    getTime :: B.ByteString -> Either String Time
    getTime line = getDatetime line

    toTimestamp :: Time -> Timestamp
    toTimestamp = init . show . utcTimeToPOSIXSeconds . fromJust . (parseTime defaultTimeLocale "%F %T") . SB.unpack 

    isNewer :: RingBuffer -> Time -> Bool
    isNewer buf time
      | C.isEmpty buf = True
      | fst (fromJust (C.focus buf)) < time = True
      | otherwise = False

    -- focus on buf is always on the newest element
    insertIntoBuf :: MetricState -> RingBuffer -> Time -> RingBuffer
    insertIntoBuf metricState buf time
      | isNewer buf time = C.insertL (time, metricState) buf
      | fst (fromJust (C.focus buf)) == time = C.update (time, combine (snd $ fromJust $ C.focus buf) metricState) buf
      | otherwise = C.rotL $ rotateToOldest $ insertIntoBuf metricState (C.rotL buf) time

    downSizeBuf :: RingBuffer -> (RingBuffer, Maybe (Time, MetricState))
    downSizeBuf buf
      | C.size buf <= maxSize = (buf, Nothing)
      | otherwise             = (C.removeL bufAtOldest, C.focus bufAtOldest)
      where
        bufAtOldest = rotateToOldest buf

    rotateToOldest :: RingBuffer -> RingBuffer
    rotateToOldest buf = rotN (findOldest 0 (rightElements buf)) buf
      where
        findOldest n (x1:x2:xs)
          | fst x1 > fst x2 = n+1
          | otherwise       = findOldest (n+1) (x2:xs)
        findOldest n _ = n

    process :: [B.ByteString] -> State RingBuffer [Results]
    process [] = do
      buf <- get
      return $ map (uncurry toResults . (\(x,y) -> (toTimestamp x, y))) (C.toList buf)
    process (i:is) = case getTime i of
      Left _ -> process is
      Right time -> do
        buf <- get
        let biggerBuf = insertIntoBuf (metric [i]) buf time
        let (newBuf, readyElem) = downSizeBuf biggerBuf
        put newBuf
        rest <- process is
        return $ case readyElem of
          Just (time', metricState) -> toResults (toTimestamp time') metricState
          Nothing   -> []
          : rest
      
  
