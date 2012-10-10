import Config
import Carbon
import Metric
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B
import System.Console.GetOpt
import Control.Concurrent
import Control.Exception (finally)
import System.Exit
import Network.Fancy
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Data.List
import Buffer (getResultsBufferedBySecond)
import Data.Time.LocalTime (getCurrentTimeZone, TimeZone)

type Line = String

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
outputFlagToAction Debug                = flip ($) stdout
outputFlagToAction (Graphite hostport)  = withStream $ IP host (read port)
  where
    (host:_:port:_) = groupBy (\a b -> a /= ':' && b /= ':') hostport -- bleh
outputFlagToAction _                    = error $ "Something has gone horribly wrong."

produceOutput :: IMetricState a => TimeZone -> Metric a -> [B.ByteString] -> Handle -> IO ()
produceOutput tz metric input handle = mapM_ (flip sendToCarbon handle) result
  where
    result = concat $ getResultsBufferedBySecond tz 10 input metric

children :: MVar [a]
children = unsafePerformIO $ newMVar []

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar:childs)
  forkIO (io `finally` putMVar mvar ())


main :: IO ()
main = do
  args <- getArgs
  (opts, _) <- parseOptions args
  if Help `elem` opts then putStrLn (usageInfo header options) >> exitSuccess else return ()
  let outputActions = map outputFlagToAction $ filter isOutputFlag opts
  if length outputActions == 0 then ioError (userError "Please specify at least one output destination (-g or -d)") else return ()
  let metrics = map makeMetric config

  tz <- getCurrentTimeZone
  input <- B.getContents

  let threads = map (\metric -> mapM_ ($ produceOutput tz metric (B.split '\n' input)) outputActions) metrics :: [IO ()]

  mapM_ forkChild threads

  waitForChildren