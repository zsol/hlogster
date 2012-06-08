import Carbon
import Input
import Metric1

type Line = String

-- inp is a simulation of a lof file stream
s = "2012-06-01 01:23:03,657 app6 usage INFO 264:presentations_delete User 10179708 deleted presentation 30334553 (owner: 10179708, title: Frank Macfarlane Burnet, public: 0, version: 14, created: 2012-05-"
s2 = "2012-06-02 18:34:43,952 domU-12-31-38-00-C1-72 storage INFO 13:log_timing timing read_preview 10.852 0701.static.prezi.com 30588598"
inp = (take 5 (repeat s)) ++ (take 5 (repeat s2))

metric1 :: Metric
metric1 = ("presentations_delete", (count_events "usage" "presentations_delete"))
metric2 = ("feature.label_zoom", (count_events "feature" "label_zoom"))


-- processList' lines metric = sendTo localhost $ showSecond $ metric lines
--   where
--     showSecond (x,y) = (x, show y)

applyMetric :: [[String]] -> Metric -> (String, Int)
applyMetric lines (name, fun) = (name, fun lines)

--applyMetric tokenized_lines metric = showSecond (metric tokenized_lines) where
--   showSecond (x,y) = (x, show y)

metrics = [metric1, metric2]

countMetric input = 
	(map (applyMetric tokenized_lines) metrics) where
		tokenized_lines = map words input

main = do
  print (countMetric inp)
  --input <- getContents
  --processList (lines input) metrics (sendTo localhost)
