{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module DMExec (
        dmExecCmd
    ) where

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
import DeviceMapper.Types

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import System.Exit
import System.IO
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

   -- This is a stack of objects, pairs are added to the head
   _vmObj :: [Object]
}

makeLenses ''VMState

newState :: Fd -> I.Program -> VMState
newState ctrl code = VMState {
    _vmCtrl = ctrl,
    _vmCode = code,
    _vmPC = 0,
    _vmFrames = newStack,
    _vmObj = [H.empty]}

type VM = StateT VMState IO

-- FIXME: we need a way of producing structured output (JSON)
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
setPC pc = modify (\vm -> vm & vmPC ^~ pc)

nextInstr :: VM I.Instruction
nextInstr = getInstr <* incPC

dm :: (Fd -> IO (IoctlResult a)) -> VM (IoctlResult a)
dm fn = getCtrl >>= (lift . fn)

noResult :: (Fd -> IO (IoctlResult ())) -> VM (Maybe Int)
noResult fn = (dm fn) >> return Nothing

-- FIXME: handle error
insertPair :: Text -> Value -> [Object] -> [Object]
insertPair _ _ [] = error "can't insert pair"
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
popObject _ [_] = error "can't pop object"
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
    return (exitCode, Object . head $ vm ^. vmObj)

--------------------------------------------------

errorTarget len = TableLine "error" len ""

usage :: IO ExitCode
usage = do
    hPutStrLn stderr "usage: dmexec <program file>"
    return $ ExitFailure 1

readProgram :: FilePath -> IO (Maybe I.Program)
readProgram path = LS.readFile path >>= (return . decode)

dmExecCmd :: [Text] -> IO ExitCode
dmExecCmd args = do
    if length args /= 1
    then usage
    else do
        mprg<- readProgram . T.unpack . head $ args
        case mprg of
            Nothing -> do
                hPutStrLn stderr "Invalid program"
                return $ ExitFailure 1
            Just program -> do
                (exitCode, obj) <- runVM program
                LS.putStrLn . encodePretty $ obj
                if exitCode == 0
                then return ExitSuccess
                else return $ ExitFailure exitCode

-------------------------------------------
