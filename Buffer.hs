{-# LANGUAGE FlexibleContexts #-}
module Buffer where

import           Control.Arrow              (first)
import           Control.Monad.State.Lazy
import qualified Data.ByteString.Char8      as SB
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.CircularList          as C
import           Data.Maybe                 (fromJust)
import           Data.Time.Clock.POSIX      (utcTimeToPOSIXSeconds)
import           Data.Time.Format
import           Data.Time.LocalTime        (TimeZone, localTimeToUTC)
import           Metrics.Common (IMetricState(combine, toResults), Metric, Results, Timestamp)
import           Parsers
import           System.Locale

type Time = SB.ByteString
type (RingBuffer a) = C.CList (Time, a)

empty :: RingBuffer a
empty = C.empty

size :: RingBuffer a -> Int
size = C.size

isEmpty :: RingBuffer a -> Bool
isEmpty = C.isEmpty

getResultsBufferedBySecond :: IMetricState a => TimeZone -> Int -> [B.ByteString] -> Metric a -> [Results]
getResultsBufferedBySecond tz maxSize input metric = evalState (process tz maxSize metric input) empty

runMetric :: IMetricState a => TimeZone -> Int -> Metric a -> B.ByteString -> RingBuffer a -> ([Results], RingBuffer a)
runMetric tz maxSize metric inputLine startState = runState (process tz maxSize metric [inputLine]) startState

getTime :: B.ByteString -> Either String Time
getTime = getDatetime

toTimestamp :: TimeZone -> Time -> Timestamp
toTimestamp tz = init . show . utcTimeToPOSIXSeconds . localTimeToUTC tz . fromJust . parseTime defaultTimeLocale "%F %T" . SB.unpack

isNewer :: Time -> RingBuffer a -> Bool
isNewer time buf
  | isEmpty buf = True
  | fst (fromJust (C.focus buf)) < time = True
  | otherwise = False

isOlder :: Time -> RingBuffer a -> Bool
isOlder time buf
  | isEmpty buf = True
  | fst (fromJust (C.focus (C.rotR buf))) > time = True
  | otherwise = False

-- focus on buf is always on the newest element
insertIntoBuf :: IMetricState a => a -> RingBuffer a -> Time -> RingBuffer a
insertIntoBuf metricState buf time
  | time `isNewer` buf = insert buf
  | fst (fromJust (C.focus buf)) == time = update buf
  | time `isOlder` buf = C.rotL $ insert buf
  | otherwise = rotateToNewest $ insertHelper (C.rotL buf)
  where
    insert = C.insertL (time, metricState)
    update b = C.update (time, combine (snd $ fromJust $ C.focus b) metricState) b
    insertHelper rotBuf -- `time` is not newer nor older than buffer
      | rotFocusTime == time = update rotBuf
      | rotFocusTime < time  = insert rotBuf
      | otherwise            = insertHelper (C.rotL rotBuf)
      where
        rotFocusTime = fst (fromJust (C.focus rotBuf))

downSizeBuf :: RingBuffer a -> Int -> (RingBuffer a, Maybe (Time, a))
downSizeBuf buf maxSize
  | size buf <= maxSize = (buf, Nothing)
  | otherwise             = (C.removeL bufAtOldest, C.focus bufAtOldest)
  where
    bufAtOldest = rotateToOldest buf

rotateToNewest :: RingBuffer a -> RingBuffer a
rotateToNewest = C.rotL . rotateToOldest

rotateToOldest :: RingBuffer a -> RingBuffer a
rotateToOldest buf = C.rotN (findOldest 0 (C.rightElements buf)) buf
  where
    findOldest n (x1:x2:xs)
      | fst x1 > fst x2 = n+1
      | otherwise       = findOldest (n+1) (x2:xs)
    findOldest n _ = n

process :: IMetricState a => TimeZone -> Int -> Metric a -> [B.ByteString] -> State (RingBuffer a) [Results]
process tz _ _ [] = do
  buf <- get
  return $ map (uncurry toResults . first (toTimestamp tz)) (C.toList buf)
process tz maxSize metric (i:is) = case getTime i of
  Left _ -> process tz maxSize metric is
  Right time -> do
    buf <- get
    let (newBuf, readyElem) = processLine maxSize metric i time buf
    put newBuf
    rest <- process tz maxSize metric is
    return $ case readyElem of
      Just (time', metricState) -> toResults (toTimestamp tz time') metricState
      Nothing   -> []
      : rest

processLine :: IMetricState a => Int -> Metric a -> B.ByteString -> Time -> RingBuffer a -> (RingBuffer a, Maybe (Time, a))
processLine maxSize metric line time buf
  | size buf < maxSize = (expandedBuf, Nothing)
  | time `isOlder` buf = (buf, Nothing) -- drop very old messages if buffer is at size
  | otherwise          = downSizeBuf expandedBuf maxSize
  where
    expandedBuf = insertIntoBuf (metric [line]) buf time
