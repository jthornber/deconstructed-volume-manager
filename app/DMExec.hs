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

errorTarget len = TableLine (T.pack "error") len (T.pack "")

dmExecCmd :: [Text] -> IO ()
dmExecCmd _ =
    withControlDevice $ \ctrl -> do
        removeAll ctrl
        createDevice name uuid ctrl
        loadTable name uuid [errorTarget 1024, errorTarget 4096] ctrl
        r <- listDevices ctrl
        putStrLn . show $ r
        suspendDevice name uuid ctrl
        resumeDevice name uuid ctrl

        table <- tableTable name uuid ctrl
        putStrLn . show $ table

        status <- statusTable name uuid ctrl
        putStrLn . show $ status

        removeDevice name uuid ctrl
        return ()
    where
        name = T.pack "bar"
        uuid = T.pack ""

-------------------------------------------
