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
        createDevice name uuid ctrl
        r <- listDevices ctrl
        putStrLn . show $ r
        removeDevice name uuid ctrl
        return ()
    where
        name = T.pack "bar"
        uuid = T.pack ""

