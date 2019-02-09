module DeviceMapper.Instructions (
    Address,
    Instruction(..),
    Program,
    prettyInstruction,
    prettyProgram
    ) where

import qualified Data.Array.IArray as A

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
    Info Text DeviceId |
    Table Text DeviceId |
    BeginObject Text |
    EndObject |
    Literal Text Text |
    JmpFail Address |
    Exit Int
    deriving (Show, Eq)

type Program = A.Array Address Instruction

showT = T.pack . show

prettyInstruction :: Instruction -> Doc ()
prettyInstruction (Suspend n) = pretty "suspend" <+> pretty (show n)
prettyInstruction (Resume n) = pretty "resume" <+> pretty (show n)
prettyInstruction (Load n ts) = hsep [
    pretty "load",
    pretty (show n),
    hardline,
    pretty "    " <> (align $ pretty (show ts))
    ]
prettyInstruction (Create n) = hsep . map pretty $ [T.pack "create", showT n]
prettyInstruction (Remove n) = pretty "remove" <+> pretty (show n)

-- FIXME: use a proper pretty printer
prettyProgram :: [Instruction] -> Doc ()
prettyProgram = vcat . map prettyInstruction

