{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module DeviceMapper.Instructions (
    Address,
    Instruction(..),
    Program(..),
    programEntryPoint,
    programInstructions
    ) where

import qualified Data.Array.IArray as A

import qualified Control.Lens as LENS
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util
import DeviceMapper.Types

type Address = Int

-- Do we need a long jump instruction that restores the frame stack?
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
    BeginObject Text |
    EndObject |
    Literal Text Text |
    JmpFail Address |
    Exit Int
    deriving (Show, Eq)

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
    toJSON (BeginObject key) = op "begin-object" ["key" .= key]
    toJSON (EndObject) = op "end-object" []
    toJSON (Literal key val) = op "literal" ["key" .= key, "value" .= val]
    toJSON (JmpFail addr) = op "jmp-fail" ["address" .= addr]
    toJSON (Exit code) = op "exit" ["code" .= code]

data Program = Program {
    _programEntryPoint :: Address,
    _programInstructions :: A.Array Address Instruction
}

LENS.makeLenses ''Program

instance ToJSON Program where
    toJSON (Program pc i) = object ["entry-point" .= pc, "instructions" .= A.elems i]
    toEncoding (Program pc i) = pairs ("entry-point" .= pc <> "instructions" .= A.elems i)

