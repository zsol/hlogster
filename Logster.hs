import Carbon
import Metric
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B

import Control.Parallel.Strategies

type Line = String

concatMapM_ :: Monad m => (a1 -> m a) -> [[a1]] -> m ()
concatMapM_ f = mapM_ f . concat

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
      input <- B.getContents
      let inputLines = B.split '\n' input
      withStream localhost $ \handle -> concatMapM_ (flip sendToCarbon handle) $ concat ([ f inputLines | f <- (map (getResultsBufferedBySecond 5) metrics)] `using` parTraversable rdeepseq)

{-
 let (Right metrics) = parseConfig $ unsafePerformIO $ readFile "/Users/zsol/haskell/hlogster/metrics.json"
 let inputLines = B.split '\n' $ unsafePerformIO $ B.readFile "/Users/zsol/teststoragelog"
 let a = [ f inputLines | f <- (map (getResultsBufferedBySecond 5) metrics)]
-}