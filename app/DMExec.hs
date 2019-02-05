module DMExec (
    dmExecCmd
    ) where

import DeviceMapper.Instructions
import DeviceMapper.Ioctl

import Data.Text (Text)
import qualified Data.Text as T

dmExecCmd :: [Text] -> IO ()
dmExecCmd _ =
    withControlDevice $ \ctrl -> do
        createDevice (T.pack "bar") (T.pack "") ctrl
        r <- listDevices ctrl
        putStrLn . show $ r
        removeAll ctrl
        return ()
