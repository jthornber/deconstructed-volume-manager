module DMExec (
    dmExecCmd
    ) where

import DeviceMapper.Instructions
import DeviceMapper.Ioctl

import Data.Text (Text)
import qualified Data.Text as T

dmExecCmd :: [Text] -> IO ()
dmExecCmd _ = do
    r <- withControlDevice listDevices
    putStrLn . show $ r

{-
join :: [Text] -> Text
join = T.intercalate (T.pack " ")

-- Executes a simple command line, returns exit code
exec :: [Text] -> IO Int

suspend :: Device -> IO Int
suspend dev = exec ["dmsetup", "suspend", devPath dev]

suspend args = readProcess_ "dmsetup
    Suspend DeviceName |
    Resume DeviceName |
    Load DeviceName Text |
    Create DeviceName |
    Remove DeviceName
exec :: [Instruction] -> IO ()
exec
-}

