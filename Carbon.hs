module Carbon
       (sendTo, oam2, localhost)
       where

import System.IO (hPutStr, Handle)
import Network.Fancy
import Data.Time.Clock.POSIX

oam2 = IP "oam2.us.prezi.private" 2003
localhost = IP "localhost" 2003

sendToCarbon :: (String, String) -> Handle -> IO ()
-- sendToCarbon (key, value) handle = now >>= \time -> hPutStr handle (key ++ " " ++ value ++ " " ++ time)
sendToCarbon (key, value) handle = do
  time <- now
  hPutStr handle (key ++ " " ++ value ++ " " ++ time)

sendTo :: Address -> (String, String) -> IO ()
-- sendTo address = (withStream address) . sendToCarbon
sendTo address keyValue = withStream address (sendToCarbon keyValue)

now :: IO String
-- now = getPOSIXTime >>= return . init . show
now = do
  time <- getPOSIXTime
  return $ init $ show time

{-
try it with:
sendTo localhost ("key", "value")
-}
