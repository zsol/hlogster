module Metric (
  Metric,
  getResults,
  getResultsBufferedBySecond,
  parseConfig
  ) where

import Text.JSON
import Parsers
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as SB
import qualified Data.CircularList as C
import Control.Monad.State.Lazy
import Data.CircularList
import Data.Maybe

type Metric = [B.ByteString] -> MetricState
type Results = [(String, String)]

class IMetricState a
  where
    combine :: a -> a -> a
    toResults :: a -> Results

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

    toResults (CounterMetricState name' a) = [(name', show a)]
    toResults a = [ (name a ++ "." ++ x, show $ y a) | (x, y) <- zip ["min", "max", "avg", "count"] [min', max', avg', num']]

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

makeEventCounter :: String -> JSObject JSValue -> Result Metric
makeEventCounter nameString obj = let (!) = flip valFromObj in do
  category <- obj ! "category"
  event <- obj ! "event"
  return $ countEvents (SB.pack category) (SB.pack event) nameString

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

makeFieldCounter :: String -> JSObject JSValue -> Result Metric
makeFieldCounter nameString obj = let (!) = flip valFromObj in do
  fieldsSpec <- obj ! "fields"
  return $ countFields fieldsSpec nameString
  
makeMetric :: JSObject JSValue -> Result Metric
makeMetric obj = let (!) = flip valFromObj in do
  typeString <- obj ! "type"
  nameString <- obj ! "name"
  let factory = case typeString of
        "eventCounter" -> makeEventCounter
        "fieldCounter" -> makeFieldCounter
        _ -> \_ _ -> Error "unknown metric type"
    in factory nameString obj

parseConfig :: String -> Either String [Metric]
parseConfig = toEither . (decode >=> mapM makeMetric)
  where
    toEither (Ok a)    = Right a
    toEither (Error a) = Left $ "Failed to parse config: " ++ a

getResults :: Metric -> [B.ByteString] -> Results
getResults metric input = toResults $ metric input

type Time = SB.ByteString
type RingBuffer = CList (Time, MetricState)

getResultsBufferedBySecond :: Int -> ([B.ByteString] -> MetricState) -> [B.ByteString] -> [Results]
getResultsBufferedBySecond maxSize metric input = evalState (process input) C.empty
  where
    getTime :: B.ByteString -> Either String Time
    getTime line = getDatetime line

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
      return $ map (toResults . snd) (C.toList buf)
    process (i:is) = case getTime i of
      Left _ -> process is
      Right time -> do
        buf <- get
        let biggerBuf = insertIntoBuf (metric [i]) buf time
        let (newBuf, readyElem) = downSizeBuf biggerBuf
        put newBuf
        rest <- process is
        return $ case readyElem of
          Just (_, metricState) -> toResults metricState
          Nothing   -> []
          : rest
      
  
