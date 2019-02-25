{-# LANGUAGE OverloadedStrings #-}

module DeviceMapper.HighLevelTypes (
    ErrorTarget(..),
    LinearTarget(..),
    DeviceOffset(..),
    StripedTarget(..),
    ThinPoolTarget(..),
    ThinTarget(..),
    CachePolicy(..),
    CacheTarget(..),
    Target(..),
    Device(..),
    devPath
    ) where

import Control.Monad
import Data.Aeson hiding (Error)
import Data.Aeson.Types hiding (Error)
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import DeviceMapper.LowLevelTypes

----------------------------------------------

data Device =
    DMDevice DeviceId |
    ExternalDevice DevicePath
    deriving (Show, Eq, Ord)

----------------------------------------------

data ErrorTarget = ErrorTarget {
    errorLen :: Sector
} deriving (Eq, Show)

instance ToJSON ErrorTarget where
    toJSON (ErrorTarget len) = object ["length" .= len]

instance FromJSON ErrorTarget where
    parseJSON (Object o) = ErrorTarget <$> o .: "length"
    parseJSON _ = mzero

----------------------------------------------

data LinearTarget = LinearTarget {
    linearDev :: Device,
    linearBegin :: Sector,
    linearEnd :: Sector
} deriving (Eq, Show)

instance ToJSON LinearTarget where
    toJSON (LinearTarget d b e) = object ["dev" .= d, "begin" .= b, "end" .= e]

instance FromJSON LinearTarget where
    parseJSON (Object o) = LinearTarget <$> o .: "dev" <*> o .: "begin" <*> o .: "end"
    parseJSON _ = mzero

----------------------------------------------

data DeviceOffset = DeviceOffset Device Sector
    deriving (Eq, Show)

instance ToJSON DeviceOffset where
    toJSON (DeviceOffset d o) = object ["dev" .= d, "offset" .= o]

instance FromJSON DeviceOffset where
    parseJSON (Object o) = DeviceOffset <$> o .: "dev" <*> o .: "offset"
    parseJSON _ = mzero

data StripedTarget = StripedTarget {
    stripedLen :: Sector,
    stripedChunkSize :: Sector,
    stripedDevs :: [DeviceOffset]
} deriving (Eq, Show)

instance ToJSON StripedTarget where
    toJSON (StripedTarget len csize devs) =
        object ["length" .= len, "chunk-size" .= csize, "devs" .= (toJSON devs)]

instance FromJSON StripedTarget where
    parseJSON (Object o) = StripedTarget <$> o .: "length" <*> o .: "chunk-size" <*> o .: "devs"
    parseJSON _ = mzero

----------------------------------------------

type Percent = Int

data ThinPoolTarget = ThinPoolTarget {
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

instance ToJSON ThinPoolTarget where
    toJSON tp = object [
        "length" .= thinPoolLen tp,
        "data-dev" .= thinPoolDataDev tp,
        "metadata-dev" .= thinPoolMetadataDev tp,
        "block-size" .= thinPoolBlockSize tp,
        "low-water-mark" .= thinPoolLowWaterMark tp,
        "zero" .= thinPoolZero tp,
        "discard" .= thinPoolDiscard tp,
        "discard-passdown" .= thinPoolDiscardPassdown tp,
        "read-only" .= thinPoolReadOnly tp,
        "error-if-no-space" .= thinPoolErrorIfNoSpace tp]


instance FromJSON ThinPoolTarget where
    parseJSON (Object o) =
        ThinPoolTarget <$>
            o .: "length" <*>
            o .: "data-dev" <*>
            o .: "metadata-dev" <*>
            o .: "block-size" <*>
            o .: "low-water-mark" <*>
            o .: "zero" <*>
            o .: "discard" <*>
            o .: "discard-passdown" <*>
            o .: "read-only" <*>
            o .: "error-if-no-space"
    parseJSON _ = mzero

----------------------------------------------

data ThinTarget = ThinTarget {
    thinLen :: Sector,
    thinPoolDev :: Device,
    thinId :: Integer,
    thinExternalOrigin :: Maybe Device
}

instance ToJSON ThinTarget where
    toJSON (ThinTarget len pool nr Nothing) =
        object ["length" .= len,
                "pool" .= pool,
                "id" .= nr]
    toJSON (ThinTarget len pool nr (Just origin)) =
        object ["length" .= len,
                "pool" .= pool,
                "id" .= nr,
                "origin" .= origin]

instance FromJSON ThinTarget where
    parseJSON (Object o) =
        ThinTarget <$> o .: "length" <*>
                       o .: "pool" <*>
                       o .: "id" <*>
                       o .:? "origin"
    parseJSON _ = mzero

----------------------------------------------

-- FIXME: add validation of the keys
data CachePolicy = CachePolicy {
    policyName :: Text,
    policyKeys :: [(Text, Text)]
}

instance ToJSON CachePolicy where
    toJSON (CachePolicy name keys) = object ["name" .= name, "keys" .= keys]

instance FromJSON CachePolicy where
    parseJSON (Object o) = CachePolicy <$> o .: "name" <*> o .: "keys"

data CacheTarget = CacheTarget {
    cacheLen :: Sector,
    cacheMetadataDev :: Device,
    cacheFastDev :: Device,
    cacheOriginDev :: Device,
    cacheBlockSize :: Sector,
    cacheFeatures :: [Text],
    cachePolicy :: CachePolicy
}

instance ToJSON CacheTarget where
    toJSON c = object [
        "length" .= cacheLen c,
        "metadata-dev" .= cacheMetadataDev c,
        "fast-dev" .= cacheFastDev c,
        "origin-dev" .= cacheOriginDev c,
        "block-size" .= cacheBlockSize c,
        "features" .= cacheFeatures c,
        "policy" .= cachePolicy c]

instance FromJSON CacheTarget where
    parseJSON (Object o) =
        CacheTarget <$>
            o .: "length" <*>
            o .: "metadata-dev" <*>
            o .: "fast-dev" <*>
            o .: "origin-dev" <*>
            o .: "block-size" <*>
            o .: "features" <*>
            o .: "policy"
    parseJSON _ = mzero

----------------------------------------------

-- FIXME: think of a better name
data Target =
    ErrorType ErrorTarget |
    LinearType LinearTarget |
    StripedType StripedTarget |
    ThinPoolType ThinPoolTarget |
    ThinType ThinTarget |
    CacheType CacheTarget

addKind :: (ToJSON a) => Text -> a -> Value
addKind k v = case toJSON v of
    (Object o) -> Object $ H.insert "kind" (String k) o
    _ -> error "not an object"

instance ToJSON Target where
    toJSON (ErrorType v) = addKind "error" v
    toJSON (LinearType v) = addKind "linear" v
    toJSON (StripedType v) = addKind "striped" v
    toJSON (ThinPoolType v) = addKind "thin-pool" v
    toJSON (ThinType v) = addKind "thin" v
    toJSON (CacheType v) = addKind "cache" v

getKind :: Object -> Parser Text
getKind o = o .: "kind"

instance FromJSON Target where
    parseJSON o@(Object v) = do
        k <- getKind v
        case k of
            "error" -> ErrorType <$> (parseJSON o :: Parser ErrorTarget)
            "linear" -> LinearType <$> parseJSON o
            "striped" -> StripedType <$> parseJSON o
            "thin-pool" -> ThinPoolType <$> parseJSON o
            "thin" -> ThinType <$> parseJSON o
            "cache" -> CacheType <$> parseJSON o
    parseJSON _ = mzero

devPath :: Device -> Text
devPath (DMDevice n) = T.append "/dev/mapper/" (devName n)
devPath (ExternalDevice p) = p

instance ToJSON Device where
    toJSON (DMDevice d) = object ["kind" .= ("dm-device" :: Text), "id" .= d]
    toJSON (ExternalDevice p) = object ["kind" .= ("external-device" :: Text), "path" .= p]

instance FromJSON Device where
    parseJSON (Object o) = do
        k <- (o .: "kind" :: Parser Text)
        case k of
            "dm-device" -> DMDevice <$> o .: "id"
            "external-device" -> ExternalDevice <$> o .: "path"
            _ -> mzero
    parseJSON _ = mzero

