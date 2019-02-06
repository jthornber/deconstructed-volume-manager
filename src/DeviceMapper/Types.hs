module DeviceMapper.Types (
    Sector,
    DeviceId(..),
    diName,
    diUUID,
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

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word

------------------------------------------

type Sector = Integer
type DevicePath = Text

data DeviceId =
    DeviceId (Maybe Text) (Maybe Text)
    deriving (Eq, Show)

emptyText :: Text
emptyText = T.pack ""

diName :: DeviceId -> Text
diName (DeviceId mn _) = fromMaybe emptyText mn

diUUID :: DeviceId -> Text
diUUID (DeviceId _ mu) = fromMaybe emptyText mu

data Device =
    DMDevice DeviceId Table |
    ExternalDevice DevicePath
    deriving (Show, Eq)

-- This is returned by device mapper
data DeviceInfo = DeviceInfo {
    devInfoDevice :: Word64,
    devInfoName :: Text
} deriving (Eq, Show)

devPath :: Device -> Text
devPath (DMDevice n _) = T.append (T.pack "/dev/mapper/") (diName n)
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

tablePrepare :: Table -> [TableLine]
tablePrepare = map targetLine . tableTargets

class ToTarget a where
    toTarget :: a -> Target

------------------------------------------
