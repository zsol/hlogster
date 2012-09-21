{-# LANGUAGE FlexibleContexts #-}
module Buffer where

import Metric
import Parsers
import Control.Monad.State.Lazy
import Data.Maybe (fromJust)
import qualified Data.CircularList as C
import Data.Time.Format
import Data.Time.Clock.POSIX
import System.Locale
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as SB


type Time = SB.ByteString
type (RingBuffer a) = C.CList (Time, a)

empty :: RingBuffer a
empty = C.empty

size :: RingBuffer a -> Int
size = C.size

isEmpty :: RingBuffer a -> Bool
isEmpty = C.isEmpty

getResultsBufferedBySecond :: IMetricState a => Int -> Metric a -> [B.ByteString] -> [Results]
getResultsBufferedBySecond maxSize metric input = evalState (process maxSize metric input) empty

getTime :: B.ByteString -> Either String Time
getTime line = getDatetime line

toTimestamp :: Time -> Timestamp
toTimestamp = init . show . utcTimeToPOSIXSeconds . fromJust . (parseTime defaultTimeLocale "%F %T") . SB.unpack 

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
  | time `isNewer` buf = C.insertL (time, metricState) buf
  | fst (fromJust (C.focus buf)) == time = C.update (time, combine (snd $ fromJust $ C.focus buf) metricState) buf
  | time `isOlder` buf = C.insertR (time, metricState) buf
  | otherwise = rotateToNewest $ insertIntoBuf metricState (C.rotL buf) time

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

process :: IMetricState a => Int -> Metric a -> [B.ByteString] -> State (RingBuffer a) [Results]
process _ _ [] = do
  buf <- get
  return $ map (uncurry toResults . (\(x,y) -> (toTimestamp x, y))) (C.toList buf)
process maxSize metric (i:is) = case getTime i of
  Left _ -> process maxSize metric is
  Right time -> do
    buf <- get
    let (newBuf, readyElem) = processLine maxSize metric i time buf
    put newBuf
    rest <- process maxSize metric is
    return $ case readyElem of
      Just (time', metricState) -> toResults (toTimestamp time') metricState
      Nothing   -> []
      : rest
      
processLine :: IMetricState a => Int -> Metric a -> B.ByteString -> Time -> RingBuffer a -> (RingBuffer a, Maybe (Time, a))
processLine maxSize metric line time buf
  | size buf < maxSize = (expandedBuf, Nothing)
  | time `isOlder` buf = (buf, Nothing) -- drop very old messages if buffer is at size
  | otherwise          = downSizeBuf expandedBuf maxSize
  where
    expandedBuf = insertIntoBuf (metric [line]) buf time