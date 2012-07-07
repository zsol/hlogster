import Carbon
import Metric
import System.Environment

type Line = String

main :: IO ()
main = do
  args <- getArgs
  if null args then error "Please specify a config file as the single argument" else return ()
  config <- readFile $ head args
  case parseConfig config of
    Left message -> error message
    Right metrics -> run metrics
  where
    run :: [Metric] -> IO ()
    run metrics = do
      input <- getContents
      withStream localhost (\handle -> mapM_ (\metric -> sendToCarbon (name metric, show $ apply metric (lines input)) handle) metrics)

