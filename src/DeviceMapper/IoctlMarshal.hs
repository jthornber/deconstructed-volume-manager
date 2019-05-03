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

    putLoadTableIoctl,
    getLoadTableIoctl,

    putClearTableIoctl,
    getClearTableIoctl,

    putStatusTableIoctl,
    getStatusTableIoctl,

    putTableTableIoctl,
    getTableTableIoctl,

    getEnoughSpace
    ) where

import Protolude hiding (putByteString)
import DeviceMapper.LowLevelTypes
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
    then undefined -- "text field too large"
    else do
        putByteString (T.encodeUtf8 str)
        putZeroes (width - len)
    where
        len = T.length str

getFixedWidthString :: Int -> Get Text
getFixedWidthString n = getByteString n >>= (return . decodeBytes)
    where
        decodeBytes = T.decodeUtf8 . BS.pack . trim . BS.unpack
        trim = takeWhile (/= 0)

putCString :: Text -> PutM Int
putCString txt = do
    written <- loop 0 (T.unpack txt)
    putWord8 0
    return (written + 1)
    where
        -- FIXME: rewrite as a mapM_
        loop written [] = return written
        loop written (x:xs) = do
            putWord8 (fromIntegral $ ord x)
            loop (written + 1) xs

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
    putWord32host 0

    putWord64host (hdrDev hdr)

    putFixedWidthString (hdrName hdr) dmNameLen
    putFixedWidthString (hdrUUID hdr) dmUUIDLen

    -- padding
    putZeroes 7

    where
        len = (fromIntegral payloadLen) + headerStructSize
        openCount = 0

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
    hdrName = "",
    hdrUUID = ""
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

tlSectors :: TableLine -> Word64
tlSectors (TableLine _ len _) = fromIntegral len

roundUp n d = ((n + d - 1) `div` d) * d
calcPadding n d = (roundUp n d) - n

putTargetSpec :: (Word64, TableLine) -> Put
putTargetSpec (sectorStart, TableLine kind sectorSize args) = do
    putWord64host sectorStart
    putWord64host $ fromIntegral sectorSize
    putWord32host 0
    putWord32host $ fromIntegral next
    putFixedWidthString kind $ fromIntegral dmMaxTypeName
    putCString args
    putZeroes $ calcPadding argsLen 8
    where
        next = dmTargetSpecSize + fromIntegral (roundUp argsLen 8)
        argsLen = T.length args + 1

getTargetSpec :: Get (TableLine, Int)
getTargetSpec = do
    sectorStart <- getWord64host
    sectorSize <- getWord64host
    status <- getWord32host
    next <- getWord32host
    kind <- getFixedWidthString $ fromIntegral dmMaxTypeName
    args <- getCString
    return (TableLine kind (fromIntegral sectorSize) args, fromIntegral next)

calcOffsets :: [Word64] -> [Word64]
calcOffsets xs = loop [0] xs
    where
        loop acc [] = reverse acc
        loop xs@(x:_) (y:ys) = loop (x + y : xs) ys
        loop _ _ = undefined -- "can't happen"

putLoadTableIoctl :: Text -> Text -> [TableLine] -> Put
putLoadTableIoctl name uuid ts = do
    putHeader hdr dataSize
    mapM_ putTargetSpec (zip (calcOffsets $ map tlSectors ts) ts)
    where
        hdr = defaultHeader {
            hdrName = name,
            hdrUUID = uuid,
            hdrTargetCount = fromIntegral $ length ts
        }
        offsets = reverse
        dataSize = sum . map tlSize $ ts
        tlSize (TableLine kind _ args) = (fromIntegral dmTargetSpecSize) + 64 + (T.length kind) + (T.length args)

getLoadTableIoctl :: Get ()
getLoadTableIoctl = return ()

putStatusTableIoctl :: Text -> Text -> Int -> Put
putStatusTableIoctl name uuid size = do
    putHeader (devHeader name uuid 0) size
    putZeroes size

getStatusTableIoctl :: Get [TableLine]
getStatusTableIoctl = do
    hdr <- lookAhead getHeader
    skip (fromIntegral $ hdrDataOffset hdr)
    loop (hdrTargetCount hdr) []
    where
        loop 0 ts = return . reverse $ ts
        loop n ts = do
            (t, next) <- lookAhead getTargetSpec
            skip next
            loop (n - 1) (t : ts)

putTableTableIoctl :: Text -> Text -> Int -> Put
putTableTableIoctl name uuid size = do
    putHeader (devHeader name uuid dmStatusTableFlag) size
    putZeroes size

getTableTableIoctl :: Get [TableLine]
getTableTableIoctl = getStatusTableIoctl

----------------------------------------
