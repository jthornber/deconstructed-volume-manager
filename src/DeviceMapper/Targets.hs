{-# LANGUAGE OverloadedStrings #-}

module DeviceMapper.Targets (
        Error(..),
    Linear(..),
    Striped(..),
    ThinPool(..),
    Thin(..),
    CachePolicy(..),
    Cache(..)
    ) where

import DeviceMapper.Types

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

----------------------------------------------

data Striped = Striped {
    stripedLen :: Sector,
    stripedChunkSize :: Sector,
    stripedDevs :: [(Device, Sector)]
} deriving (Eq, Show)

instance ToTarget Striped where
    toTarget (Striped l c ds) = Target {
        targetLine = TableLine "stiped" l (join ([T.pack (show c)] ++
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

----------------------------------------------

-- FIXME: add validation of the keys
data CachePolicy = CachePolicy {
    policyName :: Text,
    policyKeys :: [(Text, Text)]
}

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

----------------------------------------------

data Target =
    ErrorTarget Error |
    LinearTarget Linear |
    StripedTarget Striped |
    ThinPoolTarget ThinPool |
    ThinTarget Thin |
    CacheTarget Cache

