import Carbon
import Metric1
import Prelude hiding (lines)

type Line = String

-- inp is a simulation of a log file stream
s :: String
s = "2012-06-01 01:23:03,657 app6 usage INFO 264:presentations_delete User 10179708 deleted presentation 30334553 (owner: 10179708, title: Frank Macfarlane Burnet, public: 0, version: 14, created: 2012-05-"
s2 :: String
s2 = "2012-06-02 18:34:43,952 domU-12-31-38-00-C1-72 storage INFO 13:log_timing timing read_preview 10.852 0701.static.prezi.com 30588598"
inp :: [String]
inp = replicate 5 s ++ replicate 5 s2

metric1 :: Metric
metric1 = ("presentations_delete", countEvents "usage" "presentations_delete")
metric2 :: Metric
metric2 = ("feature.label_zoom", countEvents "feature" "label_zoom") 


-- processList' lines metric = sendTo localhost $ showSecond $ metric lines
--   where
--     showSecond (x,y) = (x, show y)

applyMetric :: [[String]] -> Metric -> (String, Int)
applyMetric lines (name, fun) = (name, fun lines)

--applyMetric tokenized_lines metric = showSecond (metric tokenized_lines) where
--   showSecond (x,y) = (x, show y)

metrics :: [Metric]
metrics = [metric1, metric2]

countMetric :: [String] -> [(String, Int)]
countMetric input = 
	map (applyMetric tokenized_lines) metrics where
		tokenized_lines = map words input

main :: IO ()
main = do
  print (countMetric inp)
  input <- getContents
  processList (lines input) metrics (sendTo localhost)
