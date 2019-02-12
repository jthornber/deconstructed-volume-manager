{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module DMCompile (
    dmCompileCmd
    ) where

import qualified DeviceMapper.Instructions as I
import DeviceMapper.Types
import DeviceMapper.Targets

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Sequence (Seq, (><), (|>), (<|))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util
import GHC.Generics
import System.Exit

----------------------------------------------

-- Prototype for compiling the activation of trees of devices.

-- The device tree is implied by the following the deps
-- from a Device downwards.


newDev :: DeviceId -> Table -> Seq I.Instruction
newDev n t = S.fromList [I.Create n,
                         I.Load n (tablePrepare t),
                         I.Resume n]

reloadDev :: DeviceId -> Table -> Seq I.Instruction
reloadDev n t = S.fromList [I.Suspend n,
                            I.Load n (tablePrepare t),
                            I.Resume n]

rmDev :: DeviceId -> Seq I.Instruction
rmDev n = S.singleton $ I.Remove n

activate :: Device -> Seq I.Instruction
activate (DMDevice n t) = (msum . map activate $ tableDeps t) >< (newDev n t)
activate (ExternalDevice _) = S.empty

deactivate :: Device -> Seq I.Instruction
deactivate (DMDevice n t) = (rmDev n) >< (msum . map deactivate $ tableDeps t)
deactivate (ExternalDevice _) = S.empty

----------------------------------------------

sda, sdb :: Device
sda = ExternalDevice (T.pack "/dev/sda")
sdb = ExternalDevice (T.pack "/dev/sdb")

dmCompileCmd :: [Text] -> IO ExitCode
dmCompileCmd _ = do
    L8.putStrLn . encodePretty $ program
    return ExitSuccess

    where
        dev = DMDevice (DeviceId {devName = (T.pack "test1"), devUUID = Nothing}) table
        table = Table [toTarget (Linear sda 0 1024),
                       toTarget (Linear sdb 0 1024)]
        program = I.mkProgram 0 . toList . activate $ dev

