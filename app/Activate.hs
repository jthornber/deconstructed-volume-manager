{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Activate (
    activateCmd
    ) where

import DeviceMapper.Instructions
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

-- Prototype for compiling the activation of trees of devices.

-- The device tree is implied by the following the deps
-- from a Device downwards.


newDev :: DeviceName -> Table -> [Instruction]
newDev n t = [Create n, Load n (tablePrepare t), Resume n]

reloadDev :: DeviceName -> Table -> [Instruction]
reloadDev n t = [Suspend n, Load n (tablePrepare t), Resume n]

rmDev :: DeviceName -> [Instruction]
rmDev n = [Remove n]

activate :: Device -> [Instruction]
activate (DMDevice n t) = (concatMap activate (tableDeps t)) ++ (newDev n t)
activate (ExternalDevice _) = []

deactivate :: Device -> [Instruction]
deactivate (DMDevice n t) = (rmDev n) ++ (concatMap deactivate (tableDeps t))
deactivate (ExternalDevice _) = []

-- update :: Device -> Device -> [Instruction]
-- FIXME: finish

----------------------------------------------

----------------------------------------------

sda, sdb :: Device
sda = ExternalDevice (T.pack "/dev/sda")
sdb = ExternalDevice (T.pack "/dev/sdb")

activateCmd :: [Text] -> IO ()
activateCmd _ = do
    putStrLn "ACTIVATE:"
    putDocW 80 (indent 4 (prettyProgram $ activate dev))
    putStrLn "\n\nDEACTIVATE:"
    putDocW 80 (indent 4 (prettyProgram $ deactivate dev))
    putStrLn ""
    where
        dev = DMDevice (T.pack "test1") table
        table = Table [toTarget (Linear sda 0 1024),
                       toTarget (Linear sdb 0 1024)]

