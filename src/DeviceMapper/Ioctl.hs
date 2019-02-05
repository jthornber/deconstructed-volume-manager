{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module DeviceMapper.Ioctl (
    IoctlResult(..),
    withControlDevice,
    version,
    removeAll,
    listDevices
    ) where

import Control.Exception
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LS
import DeviceMapper.IoctlConsts
import DeviceMapper.IoctlMarshal
import DeviceMapper.Types
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import System.Posix.IO
import System.Posix.Types

------------------------------------------
-- Data.Binary.Put wants lazy bytestrings, useCStringLen wants strict.
-- I'd prefer to use strict.
toStrict :: LS.ByteString -> BS.ByteString
toStrict = BS.concat . LS.toChunks

toLazy :: BS.ByteString -> LS.ByteString
toLazy = LS.fromStrict

------------------------------------------

foreign import ccall "ioctl"
    c_ioctl :: CInt -> CInt -> Ptr CChar -> IO CInt

data IoctlBuffer

data IoctlResult a =
    IoctlFail CInt |
    IoctlNoSpace |
    IoctlSuccess a
    deriving (Eq, Show)

-- The dm-ioctl buffer gets copied in both directions, but I think this wont
-- be noticeable.

runCmd' :: Fd -> CInt -> CStringLen -> IO (IoctlResult BS.ByteString)
runCmd' (Fd controlDev) cmd s@(buffer, _) = do
        r <- c_ioctl controlDev cmd buffer
        if r /= 0
        then return $ IoctlFail r
        else do
            bs <- BS.packCStringLen s
            let space = runGet getEnoughSpace (toLazy bs)
            if space
            then return $ IoctlSuccess bs
            else return $ IoctlNoSpace

-- Returns the ioctl error code, or the populated payload
runCmd :: Fd -> CInt -> Put -> Get a -> IO (IoctlResult a)
runCmd ctrl cmd packer unpacker = do
    r <- BS.useAsCStringLen (toStrict $ runPut packer) $ runCmd' ctrl cmd
    case r of
        IoctlSuccess bs -> return . IoctlSuccess . runGet unpacker . toLazy $ bs
        IoctlNoSpace -> return $ IoctlNoSpace
        IoctlFail err -> return $ IoctlFail err

withBufferSizes :: (Int -> IO (IoctlResult a)) -> [Int] -> IO (IoctlResult a)
withBufferSizes _ [] = return IoctlNoSpace
withBufferSizes fn (x:xs) = do
    r <- fn x
    case r of
        IoctlNoSpace -> withBufferSizes fn xs
        _ -> return r

runCmdAcross :: Fd -> CInt -> (Int -> Put) -> Get a -> [Int] -> IO (IoctlResult a)
runCmdAcross ctrl cmd mkPacker unpacker sizes = withBufferSizes run sizes
    where
        run size = runCmd ctrl cmd (mkPacker size) unpacker

withControlDevice :: (Fd -> IO a) -> IO a
withControlDevice fn =
    bracket (openFd dmControlDev ReadWrite Nothing openFlags) closeFd fn
    where
        openFlags = defaultFileFlags {exclusive = True}

------------------------------------------

version :: Fd -> IO (IoctlResult Version)
version ctrl = runCmd ctrl dmVersionIoctl putVersionIoctl getVersionIoctl

removeAll :: Fd -> IO (IoctlResult ())
removeAll ctrl = runCmd ctrl dmRemoveAllIoctl putVersionIoctl getRemoveAllIoctl

bufferSizes :: [Int]
bufferSizes = [8192, 64 * 1024, 512 * 1024]

listDevices :: Fd -> IO (IoctlResult [DeviceInfo])
listDevices ctrl = do
    runCmd ctrl dmListDevicesIoctl (putListDevicesIoctl 8192) getListDevicesIoctl

    -- runCmdAcross ctrl dmListDevicesIoctl putListDevicesIoctl getListDevicesIoctl bufferSizes

------------------------------------------
