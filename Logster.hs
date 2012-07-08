import Carbon
import Metric
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B

import Control.Parallel.Strategies

type Line = String

main = do
  args <- getArgs
  if null args then error "Please specify a config file as the single argument" else return ()
  config <- readFile $ head args
  case parseConfig config of
    Left message -> error message
    Right metrics -> run metrics
  where
    run metrics = do
      input <- B.getContents
      withStream localhost (\handle -> mapM_ (flip sendToCarbon handle) (parMap rdeepseq (\metric -> (name metric, show $ apply metric (B.split '\n' input))) metrics))
