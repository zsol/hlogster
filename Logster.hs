import Carbon
import Metric

type Line = String

metric1 :: Metric
metric1 = Metric {name = "presentations_delete", apply = countEvents "usage" "presentations_delete"}
metric2 :: Metric
metric2 = Metric {name = "feature.label_zoom", apply = countEvents "feature" "label_zoom"}

metrics :: [Metric]
metrics = [metric1, metric2]

main :: IO ()
main = do
  input <- getContents
  withStream localhost (\handle -> mapM_ (\metric -> sendToCarbon (name metric, show $ apply metric (lines input)) handle) metrics)

