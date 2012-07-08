import Carbon
import Metric
import System.Environment
import qualified Data.ByteString as B
import Data.Char (ord)

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
      input <- B.getContents
      withStream localhost (\handle -> mapM_ (\metric -> sendToCarbon (name metric, show $ apply metric (B.split (fromIntegral $ ord '\n') input)) handle) metrics)

