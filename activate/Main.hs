{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List
import GHC.Generics

-- Prototype for compiling the activation of trees of devices.

type Sector = Integer
type DeviceName = String
type DevicePath = FilePath

data TableLine = TableLine Sector String deriving (Eq, Show)

data Target = Target {
    targetLine :: TableLine,
    targetDeps :: [Device]
} deriving (Eq, Show)

newtype Table = Table {
    tableTargets :: [Target]
} deriving (Eq, Show)

tableDeps :: Table -> [Device]
tableDeps = concatMap targetDeps . tableTargets

tableLinePrepare :: TableLine -> String
tableLinePrepare (TableLine len txt) = (show len) ++ " " ++ txt

tablePrepare :: Table -> String
tablePrepare = join . map (tableLinePrepare . targetLine) . tableTargets
    where
        join = concat . intersperse "\n"

data Instruction =
    Suspend DeviceName |
    Resume DeviceName |
    Load DeviceName String |
    Create DeviceName |
    Remove DeviceName
    deriving (Generic, Show, Eq)

instance ToJSON Instruction
instance FromJSON Instruction

prettyInstruction :: Instruction -> String
prettyInstruction (Suspend n) = "SUSPEND\t" ++ n
prettyInstruction (Resume n) = "RESUME\t" ++ n
prettyInstruction (Load n txt) = "LOAD\t" ++ n ++ "\n" ++ txt ++ "\n"
prettyInstruction (Create n) = "CREATE\t" ++ n
prettyInstruction (Remove n) = "REMOVE\t" ++ n

-- FIXME: use a proper pretty printer
prettyProgram :: [Instruction] -> String
prettyProgram = concat . intersperse "\n" . map prettyInstruction

data Device =
    DMDevice DeviceName Table |
    ExternalDevice DevicePath
    deriving (Show, Eq)

-- The device tree is implied by the following the deps
-- from a Device downwards.


-- Starts from scratch, we generate dev paths for anonymous nodes, so we need
-- to do this in a state monad.  We also need to combine this with the IO monad
-- since we have to check the name is unused.

devPath :: Device -> FilePath
devPath (DMDevice n _) = "/dev/mapper/" ++ n
devPath (ExternalDevice p) = p

newDev :: DeviceName -> Table -> [Instruction]
newDev n t = [Create n, Load n (tablePrepare t), Resume n]

reloadDev :: DeviceName -> Table -> [Instruction]
reloadDev n t = [Suspend n, Load n (tablePrepare t), Resume n]

rmDev :: DeviceName -> [Instruction]
rmDev n = [Remove n]

activate :: Device -> [Instruction]
activate (DMDevice n t) = (concatMap activate (tableDeps t)) ++ (newDev n t)
activate (ExternalDevice _) = []

deactivate :: Device -> [Instruction]
deactivate (DMDevice n t) = (rmDev n) ++ (concatMap deactivate (tableDeps t))
deactivate (ExternalDevice _) = []

-- update :: Device -> Device -> [Instruction]
-- FIXME: finish

----------------------------------------------

data Linear = Linear {
    linearDev :: Device,
    linearBegin :: Sector,
    linearEnd :: Sector
}

linearTarget :: Device -> Sector -> Sector -> Target
linearTarget dev b e = Target {
    targetLine = TableLine (e - b) ((devPath dev) ++ " " ++ (show b)),
    targetDeps = [dev]
}

----------------------------------------------

sda, sdb :: Device
sda = ExternalDevice "/dev/sda"
sdb = ExternalDevice "/dev/sdb"

main :: IO ()
main = do
    L8.putStrLn (encode $ activate (DMDevice "test1" table))
    where
        table = Table [linearTarget sda 0 1024, linearTarget sdb 0 1024]

