module DeviceMapper.IoctlMarshal (
    Version(..),
    putVersionIoctl,
    getVersionIoctl,

    putRemoveAllIoctl,
    getRemoveAllIoctl,

    putListDevicesIoctl,
    getListDevicesIoctl,

    putCreateDeviceIoctl,
    getCreateDeviceIoctl,

    putRemoveDeviceIoctl,
    getRemoveDeviceIoctl,

    putSuspendDeviceIoctl,
    getSuspendDeviceIoctl,

    putResumeDeviceIoctl,
    getResumeDeviceIoctl,

    putClearTableIoctl,
    getClearTableIoctl,

    getEnoughSpace
    ) where

import DeviceMapper.Types
import DeviceMapper.IoctlConsts

import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Char
import Data.Word
import qualified Data.ByteString as BS

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Foreign.C.Types

----------------------------------------

data Version = Version {
    versionMajor :: Word32,
    versionMinor :: Word32,
    versionPatch :: Word32
} deriving (Eq, Show)

data Header = Header {
    hdrVersion :: Version,
    hdrDataSize :: Word32,
    hdrDataOffset :: Word32,
    hdrTargetCount :: Word32,
    hdrFlags :: Word32,
    hdrEvent :: Word32,
    hdrDev :: Word64,
    hdrName :: Text,
    hdrUUID :: Text
} deriving (Eq, Show)

putZeroes :: Int -> Put
putZeroes n = sequence_ (replicate n zero)
    where
        zero = putWord8 0

-- FIXME: we need a better way of returning errors
putFixedWidthString :: Text -> Int -> Put
putFixedWidthString str width = do
    if len > width
    then error "text field too large"
    else do
        putByteString (T.encodeUtf8 str)
        putZeroes (width - len)
    where
        len = T.length str

getFixedWidthString :: Int -> Get Text
getFixedWidthString n = getByteString n >>= (return . decodeBytes)
    where
        decodeBytes = T.decodeUtf8 . BS.pack . trim . BS.unpack
        trim = reverse . takeWhile (/= 0) . reverse

getCString :: Get Text
getCString = loop []
    where
        loop acc = do
            c <- getWord8
            if c == 0
            then return $ T.pack (reverse acc)
            else loop (chr (fromIntegral c) : acc)

putVersion :: Version -> Put
putVersion (Version ma mi pa) = do
    putWord32host ma
    putWord32host mi
    putWord32host pa

getVersion :: Get Version
getVersion = do
    ma <- getWord32host
    mi <- getWord32host
    pa <- getWord32host
    return $ Version ma mi pa

putHeader :: Header -> Int -> Put
putHeader hdr payloadLen = do
    putVersion (hdrVersion hdr)

    putWord32host (fromIntegral len)

    putWord32host (fromIntegral headerStructSize)
    putWord32host (hdrTargetCount hdr)
    putInt32host (fromIntegral openCount)
    putWord32host (hdrFlags hdr)

    putWord32host (hdrEvent hdr)
    putWord32host padding

    putWord64host (hdrDev hdr)

    putFixedWidthString (hdrName hdr) dmNameLen
    putFixedWidthString (hdrUUID hdr) dmUUIDLen

    -- padding
    putZeroes 7

    where
        len = (fromIntegral payloadLen) + headerStructSize
        openCount = 0
        padding = 0

getHeader :: Get Header
getHeader = do
    v <- getVersion

    len <- getWord32host

    structSize <- getWord32host
    targetCount <- getWord32host
    openCount <- getInt32host
    flags <- getWord32host

    event <- getWord32host

    -- padding
    getWord32host

    dev <- getWord64host

    dmName <- getFixedWidthString dmNameLen
    dmUUID <- getFixedWidthString dmUUIDLen

    -- padding
    getByteString 7

    return $ Header {
        hdrVersion = v,
        hdrDataSize = len,
        hdrDataOffset = structSize,
        hdrTargetCount = targetCount,
        hdrFlags = flags,
        hdrEvent = event,
        hdrDev = dev,
        hdrName = dmName,
        hdrUUID = dmUUID
    }

getEnoughSpace :: Get Bool
getEnoughSpace = do
    hdr <- getHeader
    return $ ((hdrFlags hdr) .&. (fromIntegral dmBufferFullFlag)) == 0

----------------------------------------

defaultHeader :: Header
defaultHeader = Header {
    hdrVersion = Version dmVersionMajor dmVersionMinor dmVersionPatch,
    hdrDataSize = 0,
    hdrDataOffset = 0,
    hdrTargetCount = 0,
    hdrFlags = 0,
    hdrEvent = 0,
    hdrDev = 0,
    hdrName = T.pack "",
    hdrUUID = T.pack ""
}

putVersionIoctl :: Put
putVersionIoctl = putHeader defaultHeader 0

getVersionIoctl :: Get Version
getVersionIoctl = getHeader >>= (return . hdrVersion)

putRemoveAllIoctl :: Put
putRemoveAllIoctl = putHeader defaultHeader 0

getRemoveAllIoctl :: Get ()
getRemoveAllIoctl = getHeader >> return ()

-- FIXME: we're packing zeroes for the resultant payload
-- which is v. inefficient
putListDevicesIoctl :: Int -> Put
putListDevicesIoctl size = do
    putHeader defaultHeader size
    putZeroes size

getDeviceInfo :: Get (Word64, Word32, Text)
getDeviceInfo = do
    dev <- getWord64host
    offset <- getWord32host
    name <- getCString
    return $ (dev, offset, name)

getDeviceInfos :: [DeviceInfo] -> Get [DeviceInfo]
getDeviceInfos acc = do
    (dev, offset, name) <- lookAhead getDeviceInfo
    if dev == 0
    then return $ reverse acc
    else let acc' = DeviceInfo dev name : acc in
        if offset == 0
        then return . reverse $ acc'
        else do
            skip (fromIntegral offset)
            getDeviceInfos acc'

getListDevicesIoctl :: Get [DeviceInfo]
getListDevicesIoctl = do
    hdr <- lookAhead getHeader
    skip . fromIntegral . hdrDataOffset $ hdr
    getDeviceInfos []

devHeader :: Text -> Text -> Word32 -> Header
devHeader name uuid flags = do
    defaultHeader {
        hdrName = name,
        hdrUUID = uuid,
        hdrFlags = flags
    }

putDev :: Word32 -> Text -> Text -> Put
putDev flags name uuid = putHeader (devHeader name uuid flags) 0

putCreateDeviceIoctl :: Text -> Text -> Put
putCreateDeviceIoctl = putDev 0

getCreateDeviceIoctl :: Get ()
getCreateDeviceIoctl = return ()

putRemoveDeviceIoctl :: Text -> Text -> Put
putRemoveDeviceIoctl = putDev 0

getRemoveDeviceIoctl :: Get ()
getRemoveDeviceIoctl = return ()

putSuspendDeviceIoctl :: Text -> Text -> Put
putSuspendDeviceIoctl = putDev dmSuspendDeviceFlag

getSuspendDeviceIoctl :: Get ()
getSuspendDeviceIoctl = return ()

putResumeDeviceIoctl :: Text -> Text -> Put
putResumeDeviceIoctl = putDev 0

getResumeDeviceIoctl :: Get ()
getResumeDeviceIoctl = return ()

putClearTableIoctl :: Text -> Text -> Put
putClearTableIoctl = putDev 0

getClearTableIoctl :: Get ()
getClearTableIoctl = return ()

----------------------------------------
