{-# LANGUAGE FlexibleContexts #-}
module Buffer where

import Metric
import Parsers
import Control.Monad.State.Lazy
import Data.Maybe (fromJust)
import Prelude hiding (null)
import qualified Data.CircularList as C
import Data.CircularList
import Data.Time.Format
import Data.Time.Clock.POSIX
import System.Locale
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as SB


type Time = SB.ByteString
type (RingBuffer a) = CList (Time, a)

getResultsBufferedBySecond :: IMetricState a => Int -> ([B.ByteString] -> a) -> [B.ByteString] -> [Results]
getResultsBufferedBySecond maxSize metric input = evalState (process maxSize metric input) C.empty

getTime :: B.ByteString -> Either String Time
getTime line = getDatetime line

toTimestamp :: Time -> Timestamp
toTimestamp = init . show . utcTimeToPOSIXSeconds . fromJust . (parseTime defaultTimeLocale "%F %T") . SB.unpack 

isNewer :: RingBuffer a -> Time -> Bool
isNewer buf time
  | C.isEmpty buf = True
  | fst (fromJust (C.focus buf)) < time = True
  | otherwise = False

-- focus on buf is always on the newest element
insertIntoBuf :: IMetricState a => a -> RingBuffer a -> Time -> RingBuffer a
insertIntoBuf metricState buf time
  | isNewer buf time = C.insertL (time, metricState) buf
  | fst (fromJust (C.focus buf)) == time = C.update (time, combine (snd $ fromJust $ C.focus buf) metricState) buf
  | otherwise = C.rotL $ rotateToOldest $ insertIntoBuf metricState (C.rotL buf) time

downSizeBuf :: RingBuffer a -> Int -> (RingBuffer a, Maybe (Time, a))
downSizeBuf buf maxSize
  | C.size buf <= maxSize = (buf, Nothing)
  | otherwise             = (C.removeL bufAtOldest, C.focus bufAtOldest)
  where
    bufAtOldest = rotateToOldest buf

rotateToOldest :: RingBuffer a -> RingBuffer a
rotateToOldest buf = rotN (findOldest 0 (rightElements buf)) buf
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
    let biggerBuf = insertIntoBuf (metric [i]) buf time
    let (newBuf, readyElem) = downSizeBuf biggerBuf maxSize
    put newBuf
    rest <- process maxSize metric is
    return $ case readyElem of
      Just (time', metricState) -> toResults (toTimestamp time') metricState
      Nothing   -> []
      : rest
      
  

