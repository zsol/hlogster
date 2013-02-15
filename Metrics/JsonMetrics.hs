module Metrics.JsonMetrics where

import Metrics.Common
import qualified Data.ByteString.Char8           as SB
import qualified Data.ByteString.Lazy.Char8      as B
import qualified Data.Map                        as M
import Parsers (getFields)
import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

-- todo: track only items in jsonKeys
jsonMetrics :: String -> [(Int, SB.ByteString)] -> Int -> [String] -> B.ByteString -> MetricState
jsonMetrics nameString fields jsonIndex jsonKeys input = JsonMetricState $ M.fromList $ case matching input of
      Right True -> keepMetrics $ buildName $ (json, getFields [3] input)
      _          -> []
  where
    matching line = do
      fs <- getFields (map fst fields) line
      return $ and $ map (\(x,y) -> x == y) (zip fs (map snd fields))

    json = case J.decode' $ B.unwords $ drop (jsonIndex - 1) $ B.words input of
      Just x  -> x
      Nothing -> HM.empty
    buildName :: (J.Object, Either String [SB.ByteString]) -> [(T.Text, J.Value)]
    buildName (obj, Right [namePrefix]) = map (\(x, y) -> (T.pack $ (SB.unpack (fst $ SB.breakSubstring (SB.pack ".prezi.private") namePrefix)) ++ "." ++ (T.unpack x), y)) (HM.toList obj)
    buildName (obj, _) = HM.toList obj
    keepMetrics (kv:kvs) = keepMetrics kvs ++ case kv of
      (k, J.Number v) -> [(nameString ++ "." ++ T.unpack k, [show v])]
      _               -> []
    keepMetrics [] = []


