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

instance ToJSON Instruction where
    toJSON RemoveAll = object ["instr" .= ("remove-all" :: Text)]
    toJSON (List key) = object ["instr" .= ("list" :: Text), "key" .= key]
    toJSON (Create dev) = object ["instr" .= ("create" :: Text), "id" .= dev]
    toJSON (Remove dev) = object ["instr" .= ("remove" :: Text), "id" .= dev]
    toJSON (Suspend dev) = object ["instr" .= ("suspend" :: Text), "id" .= dev]
    toJSON (Resume dev) = object ["instr" .= ("resume" :: Text), "id" .= dev]
    toJSON (Load dev tls) = object ["instr" .= ("load" :: Text), "id" .= dev, "targets" .= tls]
    toJSON (InfoQ key dev) = object ["instr" .= ("info" :: Text), "id" .= dev, "key" .= key]
    toJSON (TableQ key dev) = object ["instr" .= ("table" :: Text), "id" .= dev, "key" .= key]
    toJSON (BeginObject key) = object ["instr" .= ("begin-object" :: Text), "key" .= key]
    toJSON (EndObject) = object ["instr" .= ("end-object" :: Text)]
    toJSON (Literal key val) = object ["instr" .= ("literal" :: Text), "key" .= key, "value" .= val]
    toJSON (JmpFail addr) = object ["instr" .= ("jmp-fail" :: Text), "address" .= addr]
    toJSON (Exit code) = object ["instr" .= ("exit" :: Text), "code" .= code]

data Program = Program {
    _programEntryPoint :: Address,
    _programInstructions :: A.Array Address Instruction
}

LENS.makeLenses ''Program

instance ToJSON Program where
    toJSON (Program pc i) = object ["entry-point" .= pc, "instructions" .= A.elems i]
    toEncoding (Program pc i) = pairs ("entry-point" .= pc <> "instructions" .= A.elems i)

