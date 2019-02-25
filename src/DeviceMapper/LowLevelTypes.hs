{-# LANGUAGE OverloadedStrings #-}

module DeviceMapper.LowLevelTypes (
    Sector,
    DeviceId(..),
    DevicePath,
    DeviceInfo(..),
    TableLine(..)


    ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word

------------------------------------------

type Sector = Integer
type DevicePath = Text

data DeviceId = DeviceId {
    devName :: Text,
    devUUID :: Maybe Text
} deriving (Eq, Show)

instance ToJSON DeviceId where
    toJSON (DeviceId n u) = object $ ["name" .= n] ++ uuid
        where
            uuid = if isJust u
                   then ["uuid" .= fromJust u]
                   else []

instance FromJSON DeviceId where
    parseJSON (Object v) = DeviceId <$> v .: "name" <*> v .:? "uuid"
    parseJSON _ = mzero



-- This is returned by device mapper
data DeviceInfo = DeviceInfo {
    devInfoDevice :: Word64,
    devInfoName :: Text
} deriving (Eq, Show)

instance ToJSON DeviceInfo where
    toJSON (DeviceInfo d n) = object ["device" .= d, "name" .= n]
    toEncoding (DeviceInfo d n) = pairs ("dev" .= d <> "name" .= n)

data TableLine = TableLine Text Sector Text deriving (Eq, Show)

instance ToJSON TableLine where
    toJSON (TableLine kind len args) =
        object ["type" .= kind, "size" .= len, "args" .= args]
    toEncoding (TableLine kind len args) =
        pairs ("type" .= kind <> "size" .= len <> "args" .= args)

instance FromJSON TableLine where
    parseJSON (Object v) = TableLine <$> v .: "type" <*> v .: "size" <*> v .: "args"
    parseJSON _ = mzero

------------------------------------------
