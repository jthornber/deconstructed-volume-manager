{-# LANGUAGE TemplateHaskell #-}

module Commands.DMExec (
        dmExecCmd
    ) where

import Protolude

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Array.IArray
import qualified Data.ByteString.Lazy.Char8 as LS
import qualified Data.HashMap.Strict as H
import Data.Maybe
import qualified DeviceMapper.Instructions as I
import DeviceMapper.Ioctl
import DeviceMapper.LowLevelTypes

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import Formats.DMExec

import System.Exit
import System.IO
import System.Posix (Fd)

-------------------------------------------

data VMState = VMState {
   _vmCtrl :: Fd,
   _vmCode :: I.Program,
   _vmPC :: I.Address,
   _vmLabels :: H.HashMap Text I.Address,
   _vmLastIoctlFailed :: Bool,

   -- This is a stack of objects, pairs are added to the head
   _vmObj :: [Object]
}

makeLenses ''VMState

newState :: Fd -> I.Program -> VMState
newState ctrl code = VMState {
    _vmCtrl = ctrl,
    _vmCode = code,
    _vmPC = 0,
    _vmLabels = labels,
    _vmLastIoctlFailed = False,
    _vmObj = [H.empty]}
    where
        labels = foldr isLabel H.empty (zip (elems $ code ^. I.programInstructions) [0..])

        isLabel (I.Label name, pc) zero = H.insert name pc zero
        isLabel _ zero = zero

type VM = StateT VMState IO

-- FIXME: switch to a Seq
-- FIXME: we need a way of iterating across all devices (eg, to get deps and tables)

diUUID :: DeviceId -> Text
diUUID = fromMaybe "" . devUUID

codeLen :: I.Program -> I.Address
codeLen prog = (bounds (prog ^. I.programInstructions)) ^. _2

getCtrl :: VM Fd
getCtrl = (^. vmCtrl) <$> get

-- Returns (Exit 0) if no more instructions
getInstr :: VM I.Instruction
getInstr = do
    vm <- get
    let pc = (vm ^. vmPC) in
        if pc > (codeLen $ vm ^. vmCode)
        then return $ I.Exit 0
        else return $ (vm ^. vmCode . I.programInstructions) ! pc

incPC :: VM ()
incPC = modify (\vm -> vm & vmPC %~ (+ 1))

setPC :: I.Address -> VM ()
setPC pc = modify (\vm -> vm & vmPC .~ pc)

jmpLabel :: Text -> VM (Maybe Int)
jmpLabel name = do
    vm <- get
    case H.lookup name (vm ^. vmLabels) of
        Nothing -> undefined -- "no such label"
        Just pc -> do
            setPC pc
            return Nothing

nextInstr :: VM I.Instruction
nextInstr = getInstr <* incPC

dm :: (Fd -> IO (IoctlResult a)) -> VM (IoctlResult a)
dm fn = do
    dmResult <- getCtrl >>= (lift . fn)
    case dmResult of
        IoctlSuccess _ -> do
            modify (\vm -> vm & vmLastIoctlFailed .~ False)
            return dmResult
        _ -> do
            modify (\vm -> vm & vmLastIoctlFailed .~ True)
            return dmResult

noResult :: (Fd -> IO (IoctlResult ())) -> VM (Maybe Int)
noResult fn = (dm fn) >> return Nothing

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
            return Nothing
        _ -> return Nothing

pushObject :: [Object] -> [Object]
pushObject os = H.empty : os

-- FIXME: this error should be propogated
popObject :: Text -> [Object] -> [Object]
popObject _ [_] = undefined -- "can't pop object"
popObject key (v:o:os) = (H.insert key (Object v) o) : os

-- Returns the exit code if execution has completed.
step' :: I.Instruction -> VM (Maybe Int)
step' I.RemoveAll = noResult removeAll
step' (I.List key) = addResult key listDevices
step' (I.Create devId) = noResult $ createDevice (devName devId) (diUUID devId)
step' (I.Remove devId) = noResult $ removeDevice (devName devId) (diUUID devId)
step' (I.Suspend devId) = noResult $ suspendDevice (devName devId) (diUUID devId)
step' (I.Resume devId) = noResult $ resumeDevice (devName devId) (diUUID devId)
step' (I.Load devId table) = noResult $ loadTable (devName devId) (diUUID devId) table
step' (I.InfoQ key devId) = addResult key $ statusTable (devName devId) (diUUID devId)
step' (I.TableQ key devId) = addResult key $ tableTable (devName devId) (diUUID devId)
step' (I.Exit code) = return (Just code)
step' I.BeginObject = do
    modify $ \vm -> vm & vmObj %~ pushObject
    return Nothing
step' (I.EndObject key) = do
    modify $ \vm -> vm & vmObj %~ popObject key
    return Nothing
step' (I.Literal key val) = do
    modify $ \vm -> vm & vmObj %~ insertPair key (toJSON val)
    return Nothing
step' (I.Jmp label) = jmpLabel label
step' (I.JmpFail name) = do
    vm <- get
    if vm ^. vmLastIoctlFailed
    then jmpLabel name
    else return Nothing

step' (I.Label _) = return Nothing

-- FIXME: we need a way of reporting the ioctl error codes
step :: VM (Maybe Int)
step = (getInstr <* incPC) >>= step'

-- FIXME: print some execution stats
execCode :: VM Int
execCode = do
    mexit <- step
    case mexit of
        (Just code) -> return code
        Nothing -> execCode

runVM :: I.Program -> IO (Int, Value)
runVM code = withControlDevice $ \ctrl -> do
    (exitCode, vm) <- runStateT execCode (newState ctrl code)
    case vm ^. vmObj of
        [o] -> pure (exitCode, Object o)
        _   -> undefined -- throw exeception

-- FIXME: I don't think these belong here
tableLinePrepare :: TableLine -> Text
tableLinePrepare (TableLine n len txt) = T.concat [n, " ", T.pack $ show len, " ", txt]

--------------------------------------------------

errorTarget len = TableLine "error" len ""

pError :: Text -> IO ()
pError = pError

usage :: IO ExitCode
usage = do
    pError "usage: dm-exec <program file>"
    return $ ExitFailure 1

readProgram :: Text -> IO (Either Text I.Program)
readProgram path = parseAsm <$> T.readFile (T.unpack path)

dmExecCmd :: [Text] -> IO ExitCode
dmExecCmd [path] = do
    eprg <- readProgram path
    case eprg of
        Left err -> do
            pError "Invalid program: "
            pError err
            pure $ ExitFailure 1
        Right program -> do
            LS.putStrLn . encodePretty $ program
            pure ExitSuccess

                {-
                (exitCode, obj) <- runVM program
                LS.putStrLn . encodePretty $ obj
                if exitCode == 0
                then return ExitSuccess
                else return $ ExitFailure exitCode
                -}

-------------------------------------------
