{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Activate (
    activateCmd
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util
import GHC.Generics

-- Prototype for compiling the activation of trees of devices.

type Sector = Integer
type DeviceName = Text
type DevicePath = Text

data TableLine = TableLine Sector Text deriving (Eq, Show)

data Target = Target {
    targetLine :: TableLine,
    targetDeps :: [Device]
} deriving (Eq, Show)

newtype Table = Table {
    tableTargets :: [Target]
} deriving (Eq, Show)

tableDeps :: Table -> [Device]
tableDeps = concatMap targetDeps . tableTargets

tableLinePrepare :: TableLine -> Text
tableLinePrepare (TableLine len txt) = T.concat [
    T.pack $ show len,
    T.pack " ",
    txt]

tablePrepare :: Table -> Text
tablePrepare = join . map (tableLinePrepare . targetLine) . tableTargets
    where
        join = T.intercalate (T.pack "\n")

data Instruction =
    Suspend DeviceName |
    Resume DeviceName |
    Load DeviceName Text |
    Create DeviceName |
    Remove DeviceName
    deriving (Generic, Show, Eq)

prettyInstruction :: Instruction -> Doc ()
prettyInstruction (Suspend n) = pretty "SUSPEND" <+> pretty n
prettyInstruction (Resume n) = pretty "RESUME" <+> pretty n
prettyInstruction (Load n txt) = hsep [
    pretty "LOAD",
    pretty n,
    hardline,
    pretty "    " <> (align $ pretty txt)
    ]
prettyInstruction (Create n) = hsep . map pretty $ [T.pack "CREATE", n]
prettyInstruction (Remove n) = pretty "REMOVE" <+> pretty n

-- FIXME: use a proper pretty printer
prettyProgram :: [Instruction] -> Doc ()
prettyProgram = vcat . map prettyInstruction

data Device =
    DMDevice DeviceName Table |
    ExternalDevice DevicePath
    deriving (Show, Eq)

-- The device tree is implied by the following the deps
-- from a Device downwards.


-- Starts from scratch, we generate dev paths for anonymous nodes, so we need
-- to do this in a state monad.  We also need to combine this with the IO monad
-- since we have to check the name is unused.

devPath :: Device -> Text
devPath (DMDevice n _) = T.append (T.pack "/dev/mapper/") n
devPath (ExternalDevice p) = p

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

data Linear = Linear {
    linearDev :: Device,
    linearBegin :: Sector,
    linearEnd :: Sector
}

linearTarget :: Device -> Sector -> Sector -> Target
linearTarget dev b e = Target {
    targetLine = TableLine (e - b) (T.concat [
        devPath dev,
        T.pack " ",
        T.pack $ show b]),
    targetDeps = [dev]
}

----------------------------------------------

sda, sdb :: Device
sda = ExternalDevice (T.pack "/dev/sda")
sdb = ExternalDevice (T.pack "/dev/sdb")

activateCmd :: [Text] -> IO ()
activateCmd _ = do
    putDocW 80 (prettyProgram $ activate (DMDevice (T.pack "test1") table))
    where
        table = Table [linearTarget sda 0 1024, linearTarget sdb 0 1024]

