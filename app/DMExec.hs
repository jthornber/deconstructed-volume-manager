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


step :: Fd -> I.Instruction -> IO ()
step ctrl = step'
   where
       step' :: I.Instruction -> IO ()
       step' I.RemoveAll = do
           removeAll ctrl
           return ()
       step' I.List = do
           listDevices ctrl
           return ()
       step' (I.Create devId) = do
           createDevice (diName devId) (diUUID devId) ctrl
           return ()
       step' (I.Remove devId) = do
           removeDevice (diName devId) (diUUID devId) ctrl
           return ()
       step' (I.Suspend devId) = do
           suspendDevice (diName devId) (diUUID devId) ctrl
           return ()
       step' (I.Resume devId) = do
           resumeDevice (diName devId) (diUUID devId) ctrl
           return ()
       step' (I.Load devId table) = do
           loadTable (diName devId) (diUUID devId) table ctrl
           return ()
       step' (I.Info devId) = do
           statusTable (diName devId) (diUUID devId) ctrl
           return ()
       step' (I.Table devId) = do
           tableTable (diName devId) (diUUID devId) ctrl
           return ()

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
        bar = DeviceId (Just (T.pack "bar")) Nothing
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
