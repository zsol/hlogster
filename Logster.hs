{-# LANGUAGE CPP #-}
import           Buffer                     (getResultsBufferedBySecond, empty, runMetric)
import           Carbon
import           Config
import           Control.Concurrent
import           Control.Exception          (finally)
import           Control.Monad              (when, unless)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as SB
import           Data.List
import           Data.Time.LocalTime        (TimeZone, getCurrentTimeZone)
import           Metrics.Common
import           Metrics
import           Network
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Unsafe           (unsafePerformIO)
import Control.Proxy
import Control.Parallel.Strategies (parMap, rdeepseq)
#ifdef USE_EKG
import qualified Data.Text as T
import System.Remote.Monitoring
import System.Remote.Counter
#endif

-- this hackery with macros is needed to make ghci load this file
#ifdef MIN_VERSION_bytestring
#if MIN_VERSION_bytestring(0,10,0)
-- this is implemented properly in bytestring > 0.10
{-
#endif
#endif
import           Control.DeepSeq
import           Data.ByteString.Internal
instance NFData ByteString
#ifdef MIN_VERSION_bytestring
#if MIN_VERSION_bytestring(0,10,0)
-}
#endif
#endif

type Line = String

data Flag =
  Help |
  Graphite String |
#ifdef USE_EKG
  EKGPort String |
#endif
  Debug
  deriving (Eq, Show)

isOutputFlag :: Flag -> Bool
isOutputFlag (Graphite _) = True
isOutputFlag Debug = True
isOutputFlag _ = False

bufferSize = 10

options :: [OptDescr Flag]
options =
  [ Option "h" ["help"] (NoArg Help) "Show usage"
  , Option "g" ["graphite"] (ReqArg Graphite "HOST:PORT") "Send metric data to HOST:PORT"
  , Option "d" ["debug"] (NoArg Debug) "Send metric data to stderr"
#ifdef USE_EKG
  , Option "s" ["stats-port"] (ReqArg EKGPort "PORT") "Expose process stats on PORT"
#endif
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

outputFlagToHandle :: Flag -> IO Handle
outputFlagToHandle Debug = return stdout
outputFlagToHandle (Graphite hostport) = do
  h <- connectTo host (PortNumber $ fromIntegral portNum)
  hSetBuffering h LineBuffering
  return h
  where  
    (host:_:port:_) = groupBy (\a b -> a /= ':' && b /= ':') hostport -- bleh
    portNum = read port :: Int
outputFlagToHandle _ = error "Something has gone horribly wrong."

produceOutput :: IMetricState a => TimeZone -> Metric a -> [B.ByteString] -> Handle -> IO ()
produceOutput tz metric input handle = mapM_ (`sendToCarbon` handle) result
  where
    result = concat $ getResultsBufferedBySecond tz bufferSize input metric

children :: MVar [a]
children = unsafePerformIO $ newMVar []

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
      putMVar children ms
      _ <- takeMVar m
      waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar:childs)
  forkIO (io `finally` putMVar mvar ())

#ifdef USE_EKG
countLines :: Counter -> [B.ByteString] -> IO ()
countLines counter (_:lines) = inc counter >> countLines counter lines
countLines _ [] = return ()

lineCounterPipe :: Proxy p => Counter -> () -> Pipe p a a IO ()
lineCounterPipe counter () = runIdentityP $ forever $ do
  input <- request ()
  lift $ inc counter
  respond input
#endif

inputProducer :: Proxy p => [B.ByteString] -> () -> Producer p B.ByteString IO ()
inputProducer input () = runIdentityP $ loop input
  where
    loop (line:lines) = respond line >> loop lines
    loop [] = return ()

metricPipe :: (Proxy p, IMetricState a, Monad m, NFData a) => TimeZone -> Metric a -> () -> Pipe p B.ByteString Results m ()
metricPipe tz metric () = runIdentityP $ loop empty
  where
    loop state = do
      input <- request ()
      let (results, newState) = runMetric tz bufferSize metric input state
      _ <- respond $ concat results
      loop newState

resultConsumer :: (Proxy p) => [Handle] -> () -> Consumer p Results IO ()
resultConsumer handles () = runIdentityP $ forever $ do
  results <- request ()
  mapM_ (\r -> mapM_ (lift . sendToCarbon r) handles) results

main :: IO ()
main = do
  args <- getArgs
  (opts, _) <- parseOptions args
  when (Help `elem` opts) $ putStrLn (usageInfo header options) >> exitSuccess
  let outputActions = map outputFlagToAction $ filter isOutputFlag opts
  when (null outputActions) $ ioError (userError "Please specify at least one output destination (-g or -d)")
  handles <- mapM outputFlagToHandle $ filter isOutputFlag opts
  let metrics = map makeMetric config

  
  tz <- getCurrentTimeZone
  input <- B.getContents
  let inputLines = B.split '\n' input

#ifdef USE_EKG
  let isEKGPort (EKGPort _) = True
      isEKGPort _ = False
      ekgPort = case find isEKGPort opts of
        Just (EKGPort p) -> read p
        Nothing -> 1030

  ekg <- forkServer (SB.pack "localhost") ekgPort
  lineCounter <- getCounter (T.pack "loglines") ekg
#endif

  mapM_ (\metric -> forkChild $ runProxy $ inputProducer inputLines >->
#ifdef USE_EKG
                                           lineCounterPipe lineCounter >->
#endif
                                           metricPipe tz metric >->
                                           resultConsumer handles) metrics
  waitForChildren

  -- let threads = map (\metric -> mapM_ ($ produceOutput tz metric inputLines) outputActions) metrics :: [IO ()]

  -- mapM_ forkChild threads

  -- waitForChildren
