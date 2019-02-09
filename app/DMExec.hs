{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module DMExec (
        dmExecCmd
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Aeson
import Data.Array.IArray
import qualified Data.ByteString.Lazy.Char8 as LS
import qualified Data.HashMap.Strict as H
import Data.Maybe
import qualified DeviceMapper.Instructions as I
import DeviceMapper.Ioctl
import DeviceMapper.Types

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import System.Exit
import System.Posix (Fd)

-------------------------------------------

-- The frame stack is a list of lists.  This let's us push a new frame at the
-- start of a sub routine, and pop it at the end.

data FStack = FStack {
    frames :: [[Text]]
} deriving (Eq, Show)

-- We always start with a frame
newStack :: FStack
newStack = FStack [[]]

pushFrame :: FStack -> FStack
pushFrame (FStack fs) = FStack ([] : fs)

-- FIXME: get rid of these partial functions
popFrame :: FStack -> FStack
popFrame (FStack []) = error "No frame to pop frame from"
popFrame (FStack (x:xs)) = FStack xs

pushValue :: Text -> FStack -> FStack
pushValue txt (FStack []) = error "No frame to push value to"
pushValue txt (FStack (x:xs)) = FStack ((txt:x):xs)

popValue :: FStack -> FStack
popValue (FStack []) = error "No frame to pop value from"
popValue (FStack ([]:xs)) = error "Frame is empty, so can't pop value"
popValue (FStack ((x:xs):fs)) = FStack (xs:fs)

-------------------------------------------

data VMState = VMState {
   _vmCtrl :: Fd,
   _vmCode :: I.Program,
   _vmPC :: I.Address,
   _vmFrames :: FStack,
   _vmObj :: Object
}

makeLenses ''VMState

newState :: Fd -> I.Program -> VMState
newState ctrl code = VMState {
    _vmCtrl = ctrl,
    _vmCode = code,
    _vmPC = 0,
    _vmFrames = newStack,
    _vmObj = H.empty}

type VM = StateT VMState IO

-- FIXME: we need a way of producing structured output (JSON)
-- FIXME: switch to a Seq
-- FIXME: we need a way of iterating across all devices (eg, to get deps and tables)

diUUID :: DeviceId -> Text
diUUID = fromMaybe "" . devUUID

codeLen :: I.Program -> I.Address
codeLen code = (bounds code) ^. _2

getCtrl :: VM Fd
getCtrl = (^. vmCtrl) <$> get

-- Returns (Exit 0) if no more instructions
getInstr :: VM I.Instruction
getInstr = do
    vm <- get
    let pc = (vm ^. vmPC) in
        if pc >= (codeLen $ vm ^. vmCode)
        then return $ I.Exit 0
        else return $ (vm ^. vmCode) ! pc

incPC :: VM ()
incPC = modify (\vm -> vm & vmPC %~ (+ 1))

setPC :: I.Address -> VM ()
setPC pc = modify (\vm -> vm & vmPC ^~ pc)

nextInstr :: VM I.Instruction
nextInstr = getInstr <* incPC

dm :: (Fd -> IO (IoctlResult a)) -> VM (IoctlResult a)
dm fn = getCtrl >>= (lift . fn)

noResult :: (Fd -> IO (IoctlResult ())) -> VM (Maybe Int)
noResult fn = (dm fn) >> return Nothing

addResult :: (ToJSON a) => Text -> (Fd -> IO (IoctlResult a)) -> VM (Maybe Int)
addResult key fn = do
    r <- dm fn
    case r of
        IoctlSuccess v -> do
            modify (\vm -> vm & vmObj %~ (H.insert key (toJSON v)))
            return Nothing
        _ -> return Nothing

-- Returns the exit code if execution has completed.
step' :: I.Instruction -> VM (Maybe Int)
step' I.RemoveAll = noResult removeAll
step' (I.List key) = addResult key listDevices
step' (I.Create devId) = noResult $ createDevice (devName devId) (diUUID devId)
step' (I.Remove devId) = noResult $ removeDevice (devName devId) (diUUID devId)
step' (I.Suspend devId) = noResult $ suspendDevice (devName devId) (diUUID devId)
step' (I.Resume devId) = noResult $ resumeDevice (devName devId) (diUUID devId)
step' (I.Load devId table) = noResult $ loadTable (devName devId) (diUUID devId) table
step' (I.Info key devId) = addResult key $ statusTable (devName devId) (diUUID devId)
step' (I.Table key devId) = addResult key $ tableTable (devName devId) (diUUID devId)
step' (I.Exit code) = return (Just code)
step' (I.BeginObject key) = return Nothing
step' I.EndObject = return Nothing
step' (I.Literal key val) = return Nothing
step' (I.JmpFail pc) = setPC pc >> return Nothing

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
    return (exitCode, Object $ vm ^. vmObj)

--------------------------------------------------

errorTarget len = TableLine "error" len ""

instructions :: I.Program
instructions = array (0, length ins) (zip [0..] ins)
    where
        ins = [
            I.RemoveAll,
            I.Create bar,
            I.Load bar table,
            I.List "list",
            I.Suspend bar,
            I.Resume bar,
            I.Table "bar-table" bar,
            I.Info "bar-info" bar,
            I.Remove bar,
            I.Exit 17
            ]
        bar = DeviceId "bar" Nothing
        table = [
            errorTarget 1024,
            errorTarget 4096]

dmExecCmd :: [I.Instruction] -> IO ()
dmExecCmd _ = do
    (exitCode, obj) <- runVM instructions
    LS.putStrLn . encode $ obj
    case exitCode of
        0 -> exitSuccess
        _ -> exitWith (ExitFailure exitCode)

-------------------------------------------
