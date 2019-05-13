{-# LANGUAGE TemplateHaskell #-}

module DeviceMapper.VM (
    Address,
    Instruction(..),
    Program(..),
    programInstructions,
    mkProgram,
    runVM
    ) where

import Protolude

import qualified Data.Array.IArray as A

import DeviceMapper.Ioctl
import qualified Control.Lens as L
import Control.Lens ((^.), (%~), (.~))
import Data.Aeson
import Data.Aeson.Types
import Data.Array.IArray
import qualified Data.HashMap.Strict as H
import DeviceMapper.LowLevelTypes
import System.Posix (Fd)

----------------------------------------

type Address = Int

data Instruction =
    RemoveAll |
    List Text |
    Create DeviceId |
    Remove DeviceId |
    Suspend DeviceId |
    Resume DeviceId |
    Load DeviceId [TableLine] |
    InfoQ Text DeviceId |
    TableQ Text DeviceId |
    BeginObject |
    EndObject Text |
    Literal Text Text |
    Jmp Int |
    JmpFail Int |
    Exit Int
    deriving (Show, Eq)

-- FIXME: limit exit code to 0-255 range



op :: Text -> [Pair] -> Value
op opcode fields = object $ ["op" .= opcode] ++ fields

instance ToJSON Instruction where
    toJSON RemoveAll = op "remove-all" []
    toJSON (List key) = op "list" ["key" .= key]
    toJSON (Create dev) = op "create" ["id" .= dev]
    toJSON (Remove dev) = op "remove" ["id" .= dev]
    toJSON (Suspend dev) = op "suspend" ["id" .= dev]
    toJSON (Resume dev) = op "resume" ["id" .= dev]
    toJSON (Load dev tls) = op "load" ["id" .= dev, "targets" .= tls]
    toJSON (InfoQ key dev) = op "info" ["id" .= dev, "key" .= key]
    toJSON (TableQ key dev) = op "table" ["id" .= dev, "key" .= key]
    toJSON BeginObject = op "begin-object" []
    toJSON (EndObject key) = op "end-object" ["key" .= key]
    toJSON (Literal key val) = op "literal" ["key" .= key, "value" .= val]
    toJSON (Jmp dest) = op "jmp" ["dest" .= dest]
    toJSON (JmpFail dest) = op "jmp-fail" ["dest" .= dest]
    toJSON (Exit code) = op "exit" ["code" .= code]

getOp :: Object -> Parser Text
getOp v = v .: "op"

instance FromJSON Instruction where
    parseJSON (Object v) = do
        o <- getOp v
        case o of
            "remove-all" -> pure RemoveAll
            "list" -> List <$> v .: "key"
            "create" -> Create <$> v .: "id"
            "remove" -> Remove <$> v .: "id"
            "suspend" -> Suspend <$> v .: "id"
            "resume" -> Resume <$> v .: "id"
            "load" -> Load <$> v .: "id" <*> v .: "targets"
            "info" -> InfoQ <$> v .: "key" <*> v .: "id"
            "table" -> TableQ <$> v .: "key" <*> v .: "id"
            "begin-object" -> pure BeginObject
            "end-object" -> EndObject <$> v .: "key"
            "literal" -> Literal <$> v .: "key" <*> v .: "value"
            "jmp" -> Jmp <$> v .: "dest"
            "jmp-fail" -> JmpFail <$> v .: "dest"
            "exit" -> Exit <$> v .: "code"
            _ -> undefined --  "unknown opcode"
    parseJSON _ = mzero

data Program = Program {
    _programInstructions :: A.Array Address Instruction
} deriving (Eq, Show)

L.makeLenses ''Program

mkProgram :: [Instruction] -> Program
mkProgram code = Program {
    _programInstructions = array (0, (length code) - 1) (zip [0..] code)
}

instance ToJSON Program where
    toJSON (Program i) = object ["instructions" .= A.elems i]
    toEncoding (Program i) = pairs ("instructions" .= A.elems i)

instance FromJSON Program where
    parseJSON (Object v) = mkProgram <$> v .: "instructions"
    parseJSON _ = mzero

---------------------------------------------

-- FIXME: add a way of tracing instruction execution
-- eg, 0 list "foo" -> SUCCESS
--     1 info "bar" -> FAIL (EINVAL)
--     2 jmp 1

data VMState = VMState {
   _vmCtrl :: Fd,
   _vmCode :: Program,
   _vmPC :: Address,
   _vmLabels :: H.HashMap Text Address,
   _vmLastIoctlFailed :: Bool,

   -- This is a stack of objects, pairs are added to the head
   _vmObj :: [Object],

   _vmTrace :: Bool
}

L.makeLenses ''VMState

newState :: Fd -> Program -> VMState
newState ctrl code = VMState {
    _vmCtrl = ctrl,
    _vmCode = code,
    _vmPC = 0,
    _vmLastIoctlFailed = False,
    _vmObj = [H.empty]}

type VM = StateT VMState IO

-- FIXME: switch to a Seq
-- FIXME: we need a way of iterating across all devices (eg, to get deps and tables)

diUUID :: DeviceId -> Text
diUUID = fromMaybe "" . devUUID

codeLen :: Program -> Address
codeLen prog = (bounds (prog ^. programInstructions)) ^. L._2

getCtrl :: VM Fd
getCtrl = (^. vmCtrl) <$> get

-- Returns (Exit 0) if no more instructions
getInstr :: VM Instruction
getInstr = do
    vm <- get
    let pc = (vm ^. vmPC) in
        if pc > (codeLen $ vm ^. vmCode)
        then pure $ Exit 0
        else pure $ (vm ^. vmCode . programInstructions) ! pc

incPC :: VM ()
incPC = modify (\vm -> vm & vmPC %~ (+ 1))

-- FIXME: check the dest is in bounds?
setPC :: Address -> VM ()
setPC pc = modify (\vm -> vm & vmPC .~ pc)

dm :: (Fd -> IO (IoctlResult a)) -> VM (IoctlResult a)
dm fn = do
    dmResult <- getCtrl >>= (lift . fn)
    case dmResult of
        IoctlSuccess _ -> do
            modify (\vm -> vm & vmLastIoctlFailed .~ False)
            pure dmResult
        _ -> do
            modify (\vm -> vm & vmLastIoctlFailed .~ True)
            pure dmResult

noResult :: (Fd -> IO (IoctlResult ())) -> VM (Maybe Int)
noResult fn = (dm fn) >> pure Nothing

-- FIXME: handle error
insertPair :: Text -> Value -> [Object] -> [Object]
insertPair _ _ [] = undefined -- "can't insert pair"
insertPair k v (o:os) = (H.insert k v o) : os

addResult :: (ToJSON a) => Text -> (Fd -> IO (IoctlResult a)) -> VM (Maybe Int)
addResult key fn = do
    r <- dm fn
    case r of
        IoctlSuccess v -> do
            modify (\vm -> vm & vmObj %~ insertPair key (toJSON v))
            pure Nothing
        _ -> pure Nothing

pushObject :: [Object] -> [Object]
pushObject os = H.empty : os

-- FIXME: this error should be propogated
popObject :: Text -> [Object] -> [Object]
popObject _ [_] = undefined -- "can't pop object"
popObject key (v:o:os) = (H.insert key (Object v) o) : os

-- Returns the exit code if execution has completed.
step' :: Instruction -> VM (Maybe Int)
step' RemoveAll = noResult removeAll
step' (List key) = addResult key listDevices
step' (Create devId) = noResult $ createDevice (devName devId) (diUUID devId)
step' (Remove devId) = noResult $ removeDevice (devName devId) (diUUID devId)
step' (Suspend devId) = noResult $ suspendDevice (devName devId) (diUUID devId)
step' (Resume devId) = noResult $ resumeDevice (devName devId) (diUUID devId)
step' (Load devId table) = noResult $ loadTable (devName devId) (diUUID devId) table
step' (InfoQ key devId) = addResult key $ statusTable (devName devId) (diUUID devId)
step' (TableQ key devId) = addResult key $ tableTable (devName devId) (diUUID devId)
step' (Exit code) = pure (Just code)
step' BeginObject = do
    modify $ \vm -> vm & vmObj %~ pushObject
    pure Nothing
step' (EndObject key) = do
    modify $ \vm -> vm & vmObj %~ popObject key
    pure Nothing
step' (Literal key val) = do
    modify $ \vm -> vm & vmObj %~ insertPair key (toJSON val)
    pure Nothing
step' (Jmp dest) = setPC dest >> pure Nothing
step' (JmpFail dest) = do
    vm <- get
    if vm ^. vmLastIoctlFailed
        then setPC dest >> pure Nothing
        else pure Nothing

-- FIXME: we need a way of reporting the ioctl error codes
step :: VM (Maybe Int)
step = (getInstr <* incPC) >>= step'

-- FIXME: print some execution stats
execCode :: VM Int
execCode = do
    mexit <- step
    case mexit of
        (Just code) -> pure code
        Nothing -> execCode

runVM :: Program -> IO (Int, Value)
runVM code = withControlDevice $ \ctrl -> do
    (exitCode, vm) <- runStateT execCode (newState ctrl code)
    case vm ^. vmObj of
        [o] -> pure (exitCode, Object o)
        _   -> undefined -- throw exeception

--------------------------------------------------

