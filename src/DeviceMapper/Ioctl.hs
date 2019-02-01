{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module DeviceMapper.Ioctl (
    withControlDevice,
    version,
    removeAll
    ) where

import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LS
import DeviceMapper.IoctlConsts
import DeviceMapper.IoctlMarshal
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

-- The dm-ioctl buffer gets copied in both directions, but I think this wont
-- be noticeable.

runCmd'' :: Fd -> CInt -> CStringLen -> IO (Either CInt BS.ByteString)
runCmd'' (Fd controlDev) cmd s@(buffer, _) = do
        r <- c_ioctl controlDev cmd buffer
        if r /= 0
        then return $ Left r
        else do
            r <- BS.packCStringLen s
            return $ Right r

runCmd' :: Fd -> CInt -> BS.ByteString -> IO (Either CInt BS.ByteString)
runCmd' fd cmd payload = BS.useAsCStringLen payload $ runCmd'' fd cmd

-- Returns the ioctl error code, or the populated payload
runCmd :: Fd -> CInt -> Put -> Get a -> IO (Either CInt a)
runCmd controlDev cmd packer unpacker = do
    r <- runCmd' controlDev cmd (toStrict $ runPut packer)
    case r of
        (Left err) -> return $ Left err
        (Right bs) -> return $ Right (runGet unpacker (toLazy bs))


-- FIXME: ensure the fd is closed
withControlDevice :: (Fd -> IO a) -> IO a
withControlDevice fn = do
    fd <- openFd dmControlDev ReadWrite Nothing openFlags
    r <- fn fd
    closeFd fd
    return r
    where
        openFlags = defaultFileFlags {exclusive = True}

------------------------------------------

version :: Fd -> IO (Either CInt Version)
version ctrl = runCmd ctrl dmVersionIoctl putVersionIoctl getVersionIoctl

removeAll :: Fd -> IO (Either CInt ())
removeAll ctrl = runCmd ctrl dmRemoveAllIoctl putVersionIoctl getRemoveAllIoctl

------------------------------------------
