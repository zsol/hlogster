module Carbon
       (sendTo, oam2, oam3, localhost, withStream, sendToCarbon)
       where

import System.IO (hPutStrLn, Handle)
import Network.Fancy
import Data.Time.Clock.POSIX
import Control.Monad (liftM)
oam2 :: Address
oam2 = IP "oam2.us.prezi.private" 2003
oam3 :: Address
oam3 = IP "oam3.us.prezi.private" 2003
localhost :: Address
localhost = IP "localhost" 2003

sendToCarbon :: (String, String) -> Handle -> IO ()
sendToCarbon (key, value) handle = now >>= \time -> hPutStrLn handle (key ++ " " ++ value ++ " " ++ time)

sendTo :: Address -> (String, String) -> IO ()
sendTo address' = withStream address' . sendToCarbon

now :: IO String
now = liftM (init . show) getPOSIXTime

{-
try it with:
sendTo localhost ("key", "value")
-}
