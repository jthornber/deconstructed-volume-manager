{-# LANGUAGE OverloadedStrings #-}

module DeviceMapper.Targets (
        Error(..),
    Linear(..),
    Striped(..),
    ThinPool(..),
    Thin(..),
    CachePolicy(..),
    Cache(..),
    TargetP(..)
    ) where

import Control.Monad
import DeviceMapper.Types
import Data.Aeson hiding (Error)
import Data.Aeson.Types hiding (Error)
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

----------------------------------------------

data Error = Error {
    errorLen :: Sector
} deriving (Eq, Show)

instance ToTarget Error where
    toTarget (Error len) = Target {
        targetLine = TableLine "error" len "",
        targetDeps = []
    }

instance ToJSON Error where
    toJSON (Error len) = object ["length" .= len]

instance FromJSON Error where
    parseJSON (Object o) = Error <$> o .: "length"
    parseJSON _ = mzero

----------------------------------------------

data Linear = Linear {
    linearDev :: Device,
    linearBegin :: Sector,
    linearEnd :: Sector
} deriving (Eq, Show)

instance ToTarget Linear where
    toTarget (Linear dev b e) = Target {
        targetLine = TableLine "linear" (e - b) (T.concat [
            devPath dev,
            " ",
            T.pack $ show b]),
        targetDeps = [dev]
    }

instance ToJSON Linear where
    toJSON (Linear d b e) = object ["dev" .= d, "begin" .= b, "end" .= e]

instance FromJSON Linear where
    parseJSON (Object o) = Linear <$> o .: "dev" <*> o .: "begin" <*> o .: "end"
    parseJSON _ = mzero

----------------------------------------------

data DeviceOffset = DeviceOffset Device Sector
    deriving (Eq, Show)

instance ToJSON DeviceOffset where
    toJSON (DeviceOffset d o) = object ["dev" .= d, "offset" .= o]

instance FromJSON DeviceOffset where
    parseJSON (Object o) = DeviceOffset <$> o .: "dev" <*> o .: "offset"
    parseJSON _ = mzero

data Striped = Striped {
    stripedLen :: Sector,
    stripedChunkSize :: Sector,
    stripedDevs :: [DeviceOffset]
} deriving (Eq, Show)

instance ToTarget Striped where
    toTarget (Striped l c ds) = Target {
        targetLine = TableLine "striped" l (join ([T.pack (show c)] ++
                                         concatMap expand ds)),
        targetDeps = map (\(DeviceOffset d _) -> d) ds
    }
        where
            expand (DeviceOffset d s) = [devPath d, T.pack $ show s]
            join = T.intercalate (T.pack " ")

instance ToJSON Striped where
    toJSON (Striped len csize devs) =
        object ["length" .= len, "chunk-size" .= csize, "devs" .= (toJSON devs)]

instance FromJSON Striped where
    parseJSON (Object o) = Striped <$> o .: "length" <*> o .: "chunk-size" <*> o .: "devs"
    parseJSON _ = mzero

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
        targetLine = TableLine "thin-pool" (thinPoolLen tp) (formatTPLine tp),
        targetDeps = [thinPoolDataDev tp, thinPoolMetadataDev tp]
    }

instance ToJSON ThinPool where
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


instance FromJSON ThinPool where
    parseJSON (Object o) =
        ThinPool <$> o .: "length" <*>
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

data Thin = Thin {
    thinLen :: Sector,
    thinPoolDev :: Device,
    thinId :: Integer,
    thinExternalOrigin :: Maybe Device
}

formatTLine :: Thin -> Text
formatTLine t = join ([
    dev thinPoolDev,
    nr thinId] ++ (maybeToList (devPath <$> (thinExternalOrigin t))))
    where
        lit v = T.pack v
        nr fn = T.pack . show . fn $ t
        dev fn = devPath . fn $ t
        join = T.intercalate (T.pack " ")

instance ToTarget Thin where
    toTarget t = Target {
        targetLine = TableLine "thin" (thinLen t) (formatTLine t),
        targetDeps = [thinPoolDev t] ++ (maybeToList . thinExternalOrigin $ t)
    }

instance ToJSON Thin where
    toJSON (Thin len pool nr Nothing) =
        object ["length" .= len,
                "pool" .= pool,
                "id" .= nr]
    toJSON (Thin len pool nr (Just origin)) =
        object ["length" .= len,
                "pool" .= pool,
                "id" .= nr,
                "origin" .= origin]

instance FromJSON Thin where
    parseJSON (Object o) =
        Thin <$> o .: "length" <*>
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

data Cache = Cache {
    cacheLen :: Sector,
    cacheMetadataDev :: Device,
    cacheFastDev :: Device,
    cacheOriginDev :: Device,
    cacheBlockSize :: Sector,
    cacheFeatures :: [Text],
    cachePolicy :: CachePolicy
}

formatCLine :: Cache -> Text
formatCLine c = join [
    dev cacheMetadataDev,
    dev cacheFastDev,
    dev cacheOriginDev,
    nr cacheBlockSize,
    len (cacheFeatures c),
    join (cacheFeatures c),
    formatPolicyLine (cachePolicy c)]
    where
        lit v = T.pack v
        nr fn = T.pack . show . fn $ c
        dev fn = devPath . fn $ c
        len = T.pack . show . length
        join = T.intercalate (T.pack " ")
        formatPolicyLine (CachePolicy n ks) = join ([n] ++ (concatMap expand ks))
        expand (k, v) = [k, v]

instance ToTarget Cache where
    toTarget c = Target {
        targetLine = TableLine "cache" (cacheLen c) (formatCLine c),
        targetDeps = [
            cacheMetadataDev c,
            cacheFastDev c,
            cacheOriginDev c]
    }

instance ToJSON Cache where
    toJSON c = object [
        "length" .= cacheLen c,
        "metadata-dev" .= cacheMetadataDev c,
        "fast-dev" .= cacheFastDev c,
        "origin-dev" .= cacheOriginDev c,
        "block-size" .= cacheBlockSize c,
        "features" .= cacheFeatures c,
        "policy" .= cachePolicy c]

instance FromJSON Cache where
    parseJSON (Object o) =
        Cache <$> o .: "length" <*>
                  o .: "metadata-dev" <*>
                  o .: "fast-dev" <*>
                  o .: "origin-dev" <*>
                  o .: "block-size" <*>
                  o .: "features" <*>
                  o .: "policy"
    parseJSON _ = mzero

----------------------------------------------

-- FIXME: think of a better name
data TargetP =
    ErrorTarget Error |
    LinearTarget Linear |
    StripedTarget Striped |
    ThinPoolTarget ThinPool |
    ThinTarget Thin |
    CacheTarget Cache

addKind :: (ToJSON a) => Text -> a -> Value
addKind k v = case toJSON v of
    (Object o) -> Object $ H.insert "kind" (String k) o
    _ -> error "not an object"

instance ToJSON TargetP where
    toJSON (ErrorTarget v) = addKind "error" v
    toJSON (LinearTarget v) = addKind "linear" v
    toJSON (StripedTarget v) = addKind "striped" v
    toJSON (ThinPoolTarget v) = addKind "thin-pool" v
    toJSON (ThinTarget v) = addKind "thin" v
    toJSON (CacheTarget v) = addKind "cache" v


getKind :: Object -> Parser Text
getKind o = o .: "kind"

instance FromJSON TargetP where
    parseJSON o@(Object v) = do
        k <- getKind v
        case k of
            "error" -> ErrorTarget <$> (parseJSON o :: Parser Error)
            "linear" -> LinearTarget <$> parseJSON o
            "striped" -> StripedTarget <$> parseJSON o
            "thin-pool" -> ThinPoolTarget <$> parseJSON o
            "thin" -> ThinTarget <$> parseJSON o
            "cache" -> CacheTarget <$> parseJSON o
    parseJSON _ = mzero

