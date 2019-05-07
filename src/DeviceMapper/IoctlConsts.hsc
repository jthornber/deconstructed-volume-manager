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
    dmCreateDeviceIoctl,
    dmRemoveDeviceIoctl,
    dmSuspendDeviceIoctl,
    dmLoadTableIoctl,
    dmClearTableIoctl,
    dmStatusTableIoctl,
    dmBufferFullFlag,
    dmSuspendDeviceFlag,
    dmStatusTableFlag,
    dmMaxTypeName,
    dmTargetSpecSize
    ) where

import Protolude
import qualified Data.Text as T
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

dmDir, dmControlNode :: Text
dmDir = T.pack $ #const_str DM_DIR
dmControlNode = T.pack $ #const_str DM_CONTROL_NODE

dmControlDev :: Text
dmControlDev = mconcat ["/dev/", dmDir, "/", dmControlNode]

dmVersionIoctl :: CInt
dmVersionIoctl = #const DM_VERSION

dmRemoveAllIoctl :: CInt
dmRemoveAllIoctl = #const DM_REMOVE_ALL

dmListDevicesIoctl :: CInt
dmListDevicesIoctl = #const DM_LIST_DEVICES

dmCreateDeviceIoctl :: CInt
dmCreateDeviceIoctl = #const DM_DEV_CREATE

dmRemoveDeviceIoctl :: CInt
dmRemoveDeviceIoctl = #const DM_DEV_REMOVE

dmSuspendDeviceIoctl :: CInt
dmSuspendDeviceIoctl = #const DM_DEV_SUSPEND

dmLoadTableIoctl :: CInt
dmLoadTableIoctl = #const DM_TABLE_LOAD

dmClearTableIoctl :: CInt
dmClearTableIoctl = #const DM_TABLE_CLEAR

dmStatusTableIoctl :: CInt
dmStatusTableIoctl = #const DM_TABLE_STATUS

dmMaxTypeName :: CInt
dmMaxTypeName = #const DM_MAX_TYPE_NAME

dmBufferFullFlag :: Word32
dmBufferFullFlag = #const DM_BUFFER_FULL_FLAG

dmSuspendDeviceFlag :: Word32
dmSuspendDeviceFlag = #const DM_SUSPEND_FLAG

dmStatusTableFlag :: Word32
dmStatusTableFlag = #const DM_STATUS_TABLE_FLAG

dmTargetSpecSize :: CInt
dmTargetSpecSize = #const sizeof(struct dm_target_spec)

-----------------------------------------------
