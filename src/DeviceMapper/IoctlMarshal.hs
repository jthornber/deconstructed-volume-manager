module DeviceMapper.IoctlMarshal (
    Version(..),
    putVersionIoctl,
    getVersionIoctl,

    putRemoveAllIoctl,
    getRemoveAllIoctl
    ) where

import DeviceMapper.IoctlConsts

import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import qualified Data.ByteString as BS

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

----------------------------------------

data Version = Version {
    versionMajor :: Word32,
    versionMinor :: Word32,
    versionPatch :: Word32
} deriving (Eq, Show)

data Header = Header {
    hdrVersion :: Version,
    hdrTargetCount :: Word32,
    hdrFlags :: Word32,
    hdrEvent :: Word32,
    hdrDev :: Word64,
    hdrName :: Text,
    hdrUUID :: Text
}

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
        hdrTargetCount = targetCount,
        hdrFlags = flags,
        hdrEvent = event,
        hdrDev = dev,
        hdrName = dmName,
        hdrUUID = dmUUID
    }

----------------------------------------

defaultHeader :: Header
defaultHeader = Header {
    hdrVersion = Version dmVersionMajor dmVersionMinor dmVersionPatch,
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

----------------------------------------
