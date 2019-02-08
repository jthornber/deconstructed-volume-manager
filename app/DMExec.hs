{-# LANGUAGE TemplateHaskell #-}

module DMExec (
        dmExecCmd
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Aeson
import Data.Array.IArray
import qualified Data.ByteString.Lazy.Char8 as LS
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
   _vmFrames :: FStack
}

makeLenses ''VMState

newState :: Fd -> I.Program -> VMState
newState ctrl code = VMState {
    _vmCtrl = ctrl,
    _vmCode = code,
    _vmPC = 0,
    _vmFrames = newStack}

type VM = StateT VMState IO

-- FIXME: we need a way of producing structured output (JSON)
-- FIXME: switch to a Seq
-- FIXME: we need a way of iterating across all devices (eg, to get deps and tables)

diUUID :: DeviceId -> Text
diUUID = fromMaybe (T.pack "") . devUUID

{-
-- FIXME: should we return Exit 0 if no more instructions?
peekInstr :: VM I.Instruction
peekInstr = do
    vm <- get
    return $ (vmCode vm) ! (vmPC vm)

-- FIXME: use lens library?
vmIncPC :: VM ()
vmIncPC = modify inc
    where
        inc vm = vm {vmPC = (vmPC vm) + 1}
-}
{-
vmReadInstr :: VM I.Instruction
vmReadInstr = do
    vm <- get
    let pc = (vmPC vm)
        instr = (vmCode vm) ! pc
    in do
        put
        -}

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

nextInstr :: VM I.Instruction
nextInstr = getInstr <* incPC

dm :: (Fd -> IO (IoctlResult a)) -> VM (IoctlResult a)
dm fn = getCtrl >>= (lift . fn)

noResult :: (Fd -> IO (IoctlResult ())) -> VM (Maybe Int)
noResult fn = (dm fn) >> return Nothing

ppResult :: (ToJSON a) => (Fd -> IO (IoctlResult a)) -> VM (Maybe Int)
ppResult fn = do
    r <- dm fn
    case r of
        IoctlSuccess v -> do
            lift (LS.putStrLn . encode $ v)
            return Nothing
        _ -> return Nothing

-- Returns the exit code if execution has completed.
step' :: I.Instruction -> VM (Maybe Int)
step' I.RemoveAll = noResult removeAll
step' I.List = ppResult listDevices
step' (I.Create devId) = noResult $ createDevice (devName devId) (diUUID devId)
step' (I.Remove devId) = noResult $ removeDevice (devName devId) (diUUID devId)
step' (I.Suspend devId) = noResult $ suspendDevice (devName devId) (diUUID devId)
step' (I.Resume devId) = noResult $ resumeDevice (devName devId) (diUUID devId)
step' (I.Load devId table) = noResult $ loadTable (devName devId) (diUUID devId) table
step' (I.Info devId) = ppResult $ statusTable (devName devId) (diUUID devId)
step' (I.Table devId) = ppResult $ tableTable (devName devId) (diUUID devId)
step' (I.Exit code) = return (Just code)
step' _ = undefined

{-
step' Sub |
Ret |
Push |
Pop |
Print Text |
Label Text |
OnFail Text       step' (I.Print txt) = do
   T.putStr txt
   -}

step :: VM (Maybe Int)
step = (getInstr <* incPC) >>= step'

-- FIXME: print some execution stats
execCode :: VM Int
execCode = do
    mexit <- step
    case mexit of
        (Just code) -> return code
        Nothing -> execCode

runVM :: I.Program -> IO Int
runVM code = withControlDevice $ \ctrl -> do
    evalStateT execCode (newState ctrl code)

--------------------------------------------------

errorTarget len = TableLine (T.pack "error") len (T.pack "")

instructions :: I.Program
instructions = array (0, length ins) (zip [0..] ins)
    where
        ins = [
            I.RemoveAll,
            I.Create bar,
            I.Load bar table,
            I.List,
            I.Suspend bar,
            I.Resume bar,
            I.Table bar,
            I.Info bar,
            I.Remove bar
            ]
        bar = DeviceId (T.pack "bar") Nothing
        table = [
            errorTarget 1024,
            errorTarget 4096]

dmExecCmd :: [I.Instruction] -> IO ()
dmExecCmd _ = do
    code <- runVM instructions
    case code of
        0 -> exitSuccess
        _ -> exitWith (ExitFailure code)

{-
    withControlDevice $ \ctrl -> do
        removeAll ctrl
        createDevice name uuid ctrl
        loadTable name uuid [errorTarget 1024, errorTarget 4096] ctrl
        r <- listDevices ctrl
        putStrLn . show $ r
        suspendDevice name uuid ctrl
        resumeDevice name uuid ctrl

        table <- tableTable name uuid ctrl
        putStrLn . show $ table

        status <- statusTable name uuid ctrl
        putStrLn . show $ status

        removeDevice name uuid ctrl
        return ()
    where
        name = T.pack "bar"
        uuid = T.pack ""
        -}

-------------------------------------------
