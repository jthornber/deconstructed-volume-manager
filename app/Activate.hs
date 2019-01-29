{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Activate (
    activateCmd
    ) where

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
prettyInstruction (Suspend n) = pretty "suspend" <+> pretty n
prettyInstruction (Resume n) = pretty "resume" <+> pretty n
prettyInstruction (Load n txt) = hsep [
    pretty "load",
    pretty n,
    hardline,
    pretty "    " <> (align $ pretty txt)
    ]
prettyInstruction (Create n) = hsep . map pretty $ [T.pack "create", n]
prettyInstruction (Remove n) = pretty "remove" <+> pretty n

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

class ToTarget a where
    toTarget :: a -> Target

data Linear = Linear {
    linearDev :: Device,
    linearBegin :: Sector,
    linearEnd :: Sector
} deriving (Eq, Show)

instance ToTarget Linear where
    toTarget (Linear dev b e) = Target {
        targetLine = TableLine (e - b) (T.concat [
            devPath dev,
            T.pack " ",
            T.pack $ show b]),
        targetDeps = [dev]
    }

----------------------------------------------

data Error = Error {
    errorLen :: Sector
} deriving (Eq, Show)

instance ToTarget Error where
    toTarget (Error len) = Target {
        targetLine = TableLine len (T.pack "error"),
        targetDeps = []
    }

----------------------------------------------

data Striped = Striped {
    stripedLen :: Sector,
    stripedChunkSize :: Sector,
    stripedDevs :: [(Device, Sector)]
} deriving (Eq, Show)

instance ToTarget Striped where
    toTarget (Striped l c ds) = Target {
        targetLine = TableLine l (join ([T.pack "striped ",
                                         T.pack (show c)] ++
                                         concatMap expand ds)),
        targetDeps = map fst ds
    }
        where
            expand (d, s) = [devPath d, T.pack $ show s]
            join = T.intercalate (T.pack " ")

----------------------------------------------

type Percent = Int

data ThinPool = ThinPool {
    thinPoolLen :: Sector,
    thinPoolDataDev :: Device,
    thinPoolMetadataDev :: Device,
    thinPoolBlockSize :: Sector,
    thinPoolLowWaterMark :: Percent,
    thinPoolZero :: Bool,
    thinPoolDiscard :: Bool,
    thinPoolDiscardPassdown :: Bool,
    thinPoolReadOnly :: Bool,
    thinPoolErrorIfNoSpace :: Bool
}

formatTPLine :: ThinPool -> Text
formatTPLine tp = join ([
    lit "thin-pool",
    nr thinPoolLen,
    dev thinPoolMetadataDev,
    dev thinPoolDataDev,
    nr thinPoolBlockSize,
    nr thinPoolLowWaterMark,
    len opts] ++ opts)
    where
        opts = concatMap maybeToList [
            rflag thinPoolZero "skip-block-zeroing",
            rflag thinPoolDiscard "ignore-discard",
            flag thinPoolDiscardPassdown "no-discard-passdown",
            flag thinPoolReadOnly "read-only",
            flag thinPoolErrorIfNoSpace "error-if-no-space"]

        flag fn f = if fn tp
                    then Just . T.pack $ f
                    else Nothing

        rflag fn = flag (not . fn)

        lit v = T.pack v
        nr fn = T.pack . show . fn $ tp
        dev fn = devPath . fn $ tp
        len = T.pack . show . length
        join = T.intercalate (T.pack " ")

instance ToTarget ThinPool where
    toTarget tp = Target {
        targetLine = TableLine (thinPoolLen tp) (formatTPLine tp),
        targetDeps = [thinPoolDataDev tp, thinPoolMetadataDev tp]
    }

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

