module DeviceMapper.Instructions (
    Instruction(..),
    prettyInstruction,
    prettyProgram
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util
import DeviceMapper.Types

type Index = Integer

data Instruction =
    RemoveAll |
    List |
    Create DeviceId |
    Remove DeviceId |
    Suspend DeviceId |
    Resume DeviceId |
    Load DeviceId [TableLine] |
    Info DeviceId |
    Table DeviceId
    deriving (Show, Eq)

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

