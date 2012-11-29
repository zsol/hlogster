module Carbon
       (sendToCarbon)
       where

import           Control.Monad         (liftM)
import           Data.Time.Clock.POSIX
import           System.IO             (Handle, hPutStrLn)

sendToCarbon :: (String, String, String) -> Handle -> IO ()
sendToCarbon (key, value, "") handle = now >>= \time -> hPutStrLn handle (key ++ " " ++ value ++ " " ++ time)
sendToCarbon (key, value, time) handle = hPutStrLn handle (key ++ " " ++ value ++ " " ++ time)

now :: IO String
now = liftM (init . show) getPOSIXTime



{-
try it with:
sendTo localhost ("key", "value")
-}
