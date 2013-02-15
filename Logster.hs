{-# LANGUAGE CPP #-}
import           Buffer                     (getResultsBufferedBySecond, getTime)
import           Carbon
import           Config
import           Control.Concurrent
import           Control.Exception          (finally)
import           Control.Monad              (when)
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
import Control.Parallel.Strategies
import Control.DeepSeq
#ifdef USE_EKG
import qualified Data.Text as T
import System.Remote.Monitoring
import System.Remote.Counter
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

outputFlagToHandle :: Flag -> IO Handle
outputFlagToHandle Debug = return stdout
outputFlagToHandle (Graphite hostport) = do
  h <- connectTo host (PortNumber $ fromIntegral portNum)
  hSetBuffering h LineBuffering
  return h
  where
    (host:_:port:_) = groupBy (\a b -> a /= ':' && b /= ':') hostport -- bleh
    portNum = read port :: Int

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
#endif

main :: IO ()
main = do
  args <- getArgs
  (opts, _) <- parseOptions args
  when (Help `elem` opts) $ putStrLn (usageInfo header options) >> exitSuccess
  handles           <- mapM outputFlagToHandle $ filter isOutputFlag opts
  when (null handles) $ ioError (userError "Please specify at least one output destination (-g or -d)")
  let metrics = map makeMetric config

  tz <- getCurrentTimeZone
  input <- B.getContents
  let inputLines = {-# SCC "input" #-}B.split '\n' input

#ifdef USE_EKG
  let isEKGPort (EKGPort _) = True
      isEKGPort _ = False
      ekgPort = case find isEKGPort opts of
        Just (EKGPort p) -> read p
        Nothing -> 1030

  ekg <- forkServer (SB.pack "localhost") ekgPort
  lineCounter <- getCounter (T.pack "loglines") ekg
  -- _ <- forkChild $ countLines lineCounter inputLines
#endif

  let timestamps = map ({-# SCC "getTime" #-}force . getTime) inputLines
      statesByInput :: [[MetricState]]
      statesByInput = {-# SCC "statesByInput" #-}withStrategy (parBuffer 10 $ evalList rdeepseq) $ map ({-# SCC "sBIlambda" #-}\i -> map ($!!i) metrics) inputLines
      results :: [[[(String, String, String)]]]
      results = {-# SCC "gRBBS" #-}map (\s -> getResultsBufferedBySecond tz 10 (zip timestamps s)) ({-# SCC "transpose" #-}transpose statesByInput)
      outs :: [(String, String, String) -> IO ()]
      outs = map (flip sendToCarbon) handles
      threads :: [IO ()]
      threads = map (\o -> mapM_ (mapM_ o . concat) results) outs

  mapM_ forkChild threads

  waitForChildren
