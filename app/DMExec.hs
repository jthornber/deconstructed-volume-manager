module DMExec (
    dmExecCmd
    ) where

import DeviceMapper.Instructions
import DeviceMapper.Ioctl
import DeviceMapper.Types

import Data.Text (Text)
import qualified Data.Text as T


-------------------------------------------

-- I think we need our own DMExec monad so we can
-- handle errors nicely

dmExecCmd :: [Text] -> IO ()
dmExecCmd _ =
    withControlDevice $ \ctrl -> do
        removeAll ctrl
        createDevice name uuid ctrl
        loadTable name uuid [TableLine (T.pack "error") 1024 (T.pack "")] ctrl
        r <- listDevices ctrl
        putStrLn . show $ r
        suspendDevice name uuid ctrl
        resumeDevice name uuid ctrl
        removeDevice name uuid ctrl
        return ()
    where
        name = T.pack "bar"
        uuid = T.pack ""

-------------------------------------------
