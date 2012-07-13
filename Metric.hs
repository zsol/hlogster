module Metric (
  Metric(Metric),
  MetricFun,
  getResults,
  parseConfig
  ) where

import Control.Monad ((>=>))
import Text.JSON
import Parsers
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as SB

type MetricFun = [B.ByteString] -> [Float]
data Metric = Metric {
  apply :: MetricFun,
  names :: [String]
  }

getResults :: Metric -> [B.ByteString] -> [(String, String)]
getResults metric input = zip (names metric) $ map show $ apply metric input

toList :: t -> [t]
toList a = [a]

countEvents :: SB.ByteString -> SB.ByteString -> MetricFun
countEvents category eventId = toList . fromIntegral . length . events
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
  return $ Metric {names = [nameString], apply = countEvents (SB.pack category) (SB.pack event)}

countFields :: [(Int, SB.ByteString)] -> MetricFun
countFields spec = toList . fromIntegral . length . matchingFields
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
  return $ Metric {names = [nameString], apply = countFields fieldsSpec}
  

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
