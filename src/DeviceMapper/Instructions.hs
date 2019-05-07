{-# LANGUAGE TemplateHaskell #-}

module DeviceMapper.Instructions (
    Address,
    Instruction(..),
    Program(..),
    programInstructions,
    mkProgram
    ) where

import Protolude

import qualified Data.Array.IArray as A

import qualified Control.Lens as LENS
import Data.Aeson
import Data.Aeson.Types
import Data.Array.IArray
import DeviceMapper.LowLevelTypes

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
    Jmp Text |
    JmpFail Text |
    Label Text |
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
    toJSON BeginObject = op "begin-object" []
    toJSON (EndObject key) = op "end-object" ["key" .= key]
    toJSON (Literal key val) = op "literal" ["key" .= key, "value" .= val]
    toJSON (Jmp label) = op "jmp" ["label" .= label]
    toJSON (JmpFail label) = op "jmp-fail" ["label" .= label]
    toJSON (Label name) = op "label" ["name" .= name]
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
            "jmp" -> Jmp <$> v .: "label"
            "jmp-fail" -> JmpFail <$> v .: "label"
            "label" -> Label <$> v .: "name"
            "exit" -> Exit <$> v .: "code"
            _ -> undefined --  "unknown opcode"
    parseJSON _ = mzero

data Program = Program {
    _programInstructions :: A.Array Address Instruction
} deriving (Eq, Show)

LENS.makeLenses ''Program

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

