import           Buffer                     (getResultsBufferedBySecond)
import           Carbon
import           Config
import           Control.Concurrent
import           Control.Exception          (finally)
import           Control.Monad              (when)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.List
import           Data.Time.LocalTime        (TimeZone, getCurrentTimeZone)
import           Data.Either
import           Metrics.Common
import           Metrics
import           Network
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Unsafe           (unsafePerformIO)

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
outputFlagToAction Debug action               = action stdout
outputFlagToAction (Graphite hostport) action = do
  h <- connectTo host (PortNumber $ fromIntegral portNum)
  hSetBuffering h LineBuffering
  action h
  where
    (host:_:port:_) = groupBy (\a b -> a /= ':' && b /= ':') hostport -- bleh
    portNum = read port :: Int
outputFlagToAction _ _                        = error "Something has gone horribly wrong."

produceOutput :: IMetricState a => TimeZone -> Metric a -> [B.ByteString] -> Handle -> IO ()
produceOutput tz metric input handle = mapM_ (`sendToCarbon` handle) result
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
  when (Help `elem` opts) $ putStrLn (usageInfo header options) >> exitSuccess
  let outputActions = map outputFlagToAction $ filter isOutputFlag opts
  when (null outputActions) $ ioError (userError "Please specify at least one output destination (-g or -d)")
  let metrics = map makeMetric config
  
  tz <- getCurrentTimeZone
  input <- B.getContents

  let threads = map (\metric -> mapM_ ($ produceOutput tz metric (B.split '\n' input)) outputActions) metrics :: [IO ()]

  mapM_ forkChild threads

  waitForChildren
