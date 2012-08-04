import Config
import Carbon
import Metric
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B
import System.Console.GetOpt
import Control.Parallel.Strategies
import System.Exit
import Network.Fancy
import System.IO
import Data.List

type Line = String

concatMapM_ :: Monad m => (a1 -> m a) -> [[a1]] -> m ()
concatMapM_ f = mapM_ f . concat

data Flag =
  Help |
  Graphite String |
  Debug
  deriving (Eq, Show)

isOutputFlag :: Flag -> Bool
isOutputFlag (Graphite _) = True
isOutputFlag Debug = True
isOutputFlag _ = False

options :: [OptDescr Flag]
options =
  [ Option "h" ["help"] (NoArg Help) "Show usage"
  , Option "g" ["graphite"] (ReqArg Graphite "HOST:PORT") "Send metric data to HOST:PORT"
  , Option "d" ["debug"] (NoArg Debug) "Send metric data to stderr"
  ]

header :: String
header = "Usage: hlogster [OPTION...] CONFIG"

parseOptions :: [String] -> IO ([Flag], [String])
parseOptions argv = case getOpt Permute options argv of
  (o, n, []) -> return (o, n)
  (_, _, es) -> ioError $ userError $ concat es ++ usageInfo header options

outputFlagToAction :: Flag -> (Handle -> IO a) -> IO a
outputFlagToAction Debug                = flip ($) stderr
outputFlagToAction (Graphite hostport)  = withStream $ IP host (read port)
  where
    (host:_:port:_) = groupBy (\a b -> a /= ':' && b /= ':') hostport -- bleh
outputFlagToAction _                    = error $ "Something has gone horribly wrong."
    
main :: IO ()
main = do
  args <- getArgs
  (opts, _) <- parseOptions args
  if Help `elem` opts then putStrLn (usageInfo header options) >> exitSuccess else return ()
  let outputActions = map outputFlagToAction $ filter isOutputFlag opts
  if length outputActions == 0 then ioError (userError "Please specify at least one output destination (-g or -d)") else return ()
  let metrics = map makeMetric config

  input <- B.getContents
  let inputLines = B.split '\n' input
      resultsToHandle handle = concatMapM_ (flip sendToCarbon handle) (parMap rdeepseq (flip getResults inputLines) metrics)
      
  mapM_ ($ resultsToHandle) outputActions