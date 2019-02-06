module DMExec (
    dmExecCmd
    ) where

import Data.Maybe
import qualified DeviceMapper.Instructions as I
import DeviceMapper.Ioctl
import DeviceMapper.Types

import Data.Text (Text)
import qualified Data.Text as T

import System.Posix (Fd)

-------------------------------------------


-- I think we need our own DMExec monad so we can
-- handle errors nicely

errorTarget len = TableLine (T.pack "error") len (T.pack "")

-- FIXME: we need a way of producing structured output (JSON)
-- FIXME: switch to a Seq

diUUID :: DeviceId -> Text
diUUID = fromMaybe (T.pack "") . devUUID

showResult :: (Show a) => String -> IoctlResult a -> IO ()
showResult cmd r = putStrLn (cmd ++ ":\t" ++ (show r))

step :: Fd -> I.Instruction -> IO ()
step ctrl = step'
   where
       step' :: I.Instruction -> IO ()
       step' I.RemoveAll = do
           r <- removeAll ctrl
           showResult "remove-all" r
       step' I.List = do
           r <- listDevices ctrl
           showResult "list" r
       step' (I.Create devId) = do
           r <- createDevice (devName devId) (diUUID devId) ctrl
           showResult "create" r
       step' (I.Remove devId) = do
           r <- removeDevice (devName devId) (diUUID devId) ctrl
           showResult "remove" r
       step' (I.Suspend devId) = do
           r <- suspendDevice (devName devId) (diUUID devId) ctrl
           showResult "suspend" r
       step' (I.Resume devId) = do
           r <- resumeDevice (devName devId) (diUUID devId) ctrl
           showResult "resume" r
       step' (I.Load devId table) = do
           r <- loadTable (devName devId) (diUUID devId) table ctrl
           showResult "load" r
       step' (I.Info devId) = do
           r <- statusTable (devName devId) (diUUID devId) ctrl
           showResult "info" r
       step' (I.Table devId) = do
           r <- tableTable (devName devId) (diUUID devId) ctrl
           showResult "table" r

-- FIXME: print some execution stats
exec :: [I.Instruction] -> IO ()
exec prg = withControlDevice $ \ctrl -> do
    mapM_ (step ctrl) prg

--------------------------------------------------

instructions :: [I.Instruction]
instructions = [
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
    where
        bar = DeviceId (T.pack "bar") Nothing
        table = [
            errorTarget 1024,
            errorTarget 4096]

dmExecCmd :: [Text] -> IO ()
dmExecCmd _ = exec instructions

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
