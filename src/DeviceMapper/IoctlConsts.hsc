module DeviceMapper.IoctlConsts (
    dmVersionMajor,
    dmVersionMinor,
    dmVersionPatch,
    headerStructSize,
    dmNameLen,
    dmUUIDLen,
    dmDir,
    dmControlNode,
    dmControlDev,
    dmVersionIoctl,
    dmRemoveAllIoctl,
    dmListDevicesIoctl,
    dmBufferFullFlag
    ) where

import Data.Word
import Foreign.C.Types

#include <sys/ioctl.h>
#include <linux/dm-ioctl.h>

-- FIXME: do we need to prefix everything with 'dm'?

-----------------------------------------------

dmVersionMajor, dmVersionMinor, dmVersionPatch :: Word32
dmVersionMajor = 4
dmVersionMinor = 39
dmVersionPatch = 0

headerStructSize :: Word32
headerStructSize = #const sizeof(struct dm_ioctl)

dmNameLen, dmUUIDLen :: Int
dmNameLen = #const DM_NAME_LEN
dmUUIDLen = #const DM_UUID_LEN

dmDir, dmControlNode :: String
dmDir = #const_str DM_DIR
dmControlNode = #const_str DM_CONTROL_NODE

dmControlDev :: FilePath
dmControlDev = concat ["/dev/", dmDir, "/", dmControlNode]

dmVersionIoctl :: CInt
dmVersionIoctl = #const DM_VERSION

dmRemoveAllIoctl :: CInt
dmRemoveAllIoctl = #const DM_REMOVE_ALL

dmListDevicesIoctl :: CInt
dmListDevicesIoctl = #const DM_LIST_DEVICES

dmBufferFullFlag :: CInt
dmBufferFullFlag = #const DM_BUFFER_FULL_FLAG

-----------------------------------------------
