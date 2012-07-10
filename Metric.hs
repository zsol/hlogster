module Metric (
  Metric(Metric),
  MetricFun,
  name,
  apply,
  countEvents,
  countFields,
  parseConfig
  ) where

import Control.Monad ((>=>))
import Text.JSON
import Parsers
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as SB

type MetricFun = [B.ByteString] -> Float
data Metric = Metric {
  name :: String,
  apply :: MetricFun
  }  

countEvents :: SB.ByteString -> SB.ByteString -> MetricFun
countEvents category eventId = fromIntegral . length . events
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
  return $ Metric {name = nameString, apply = countEvents (SB.pack category) (SB.pack event)}

countFields :: [(Int, SB.ByteString)] -> MetricFun
countFields spec = fromIntegral . length . matchingFields
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
  return $ Metric {name = nameString, apply = countFields fieldsSpec}
  

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
    toEither (Error a) = Left a
