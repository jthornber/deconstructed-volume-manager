{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module DMCompile (
    dmCompileCmd
    ) where

import qualified DeviceMapper.Instructions as I
import DeviceMapper.Types
import DeviceMapper.Targets

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util
import GHC.Generics
import System.Exit

-- Prototype for compiling the activation of trees of devices.

-- The device tree is implied by the following the deps
-- from a Device downwards.


newDev :: DeviceId -> Table -> [I.Instruction]
newDev n t = [I.Create n, I.Load n (tablePrepare t), I.Resume n]

reloadDev :: DeviceId -> Table -> [I.Instruction]
reloadDev n t = [I.Suspend n, I.Load n (tablePrepare t), I.Resume n]

rmDev :: DeviceId -> [I.Instruction]
rmDev n = [I.Remove n]

activate :: Device -> [I.Instruction]
activate (DMDevice n t) = (concatMap activate (tableDeps t)) ++ (newDev n t)
activate (ExternalDevice _) = []

deactivate :: Device -> [I.Instruction]
deactivate (DMDevice n t) = (rmDev n) ++ (concatMap deactivate (tableDeps t))
deactivate (ExternalDevice _) = []

-- update :: Device -> Device -> [I.Instruction]
-- FIXME: finish

----------------------------------------------

----------------------------------------------

sda, sdb :: Device
sda = ExternalDevice (T.pack "/dev/sda")
sdb = ExternalDevice (T.pack "/dev/sdb")

dmCompileCmd :: [Text] -> IO ExitCode
dmCompileCmd _ = do
    putStrLn "ACTIVATE:"
    -- putDocW 80 (indent 4 (I.prettyProgram $ activate dev))
    putStrLn "\n\nDEACTIVATE:"
    -- putDocW 80 (indent 4 (I.prettyProgram $ deactivate dev))
    putStrLn ""
    return ExitSuccess
    where
        dev = DMDevice (DeviceId {devName = (T.pack "test1"), devUUID = Nothing}) table
        table = Table [toTarget (Linear sda 0 1024),
                       toTarget (Linear sdb 0 1024)]

