module DeviceMapper.Types (
    Sector,
    DeviceName,
    DevicePath,
    Device(..),
    DeviceInfo(..),
    devPath,
    TableLine(..),
    Target(..),
    Table(..),
    tableDeps,
    tableLinePrepare,
    tablePrepare,
    ToTarget(..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word

------------------------------------------

type Sector = Integer
type DeviceName = Text
type DevicePath = Text

data Device =
    DMDevice DeviceName Table |
    ExternalDevice DevicePath
    deriving (Show, Eq)

-- This is returned by device mapper
data DeviceInfo = DeviceInfo {
    devInfoDevice :: Word64,
    devInfoName :: Text
} deriving (Eq, Show)

devPath :: Device -> Text
devPath (DMDevice n _) = T.append (T.pack "/dev/mapper/") n
devPath (ExternalDevice p) = p

data TableLine = TableLine Text Sector Text deriving (Eq, Show)

data Target = Target {
    targetLine :: TableLine,
    targetDeps :: [Device]
} deriving (Eq, Show)

newtype Table = Table {
    tableTargets :: [Target]
} deriving (Eq, Show)

tableDeps :: Table -> [Device]
tableDeps = concatMap targetDeps . tableTargets

-- FIXME: I don't think these belong here
tableLinePrepare :: TableLine -> Text
tableLinePrepare (TableLine n len txt) = T.concat [
    n,
    T.pack " ",
    T.pack $ show len,
    T.pack " ",
    txt]

tablePrepare :: Table -> Text
tablePrepare = join . map (tableLinePrepare . targetLine) . tableTargets
    where
        join = T.intercalate (T.pack "\n")

class ToTarget a where
    toTarget :: a -> Target

------------------------------------------
