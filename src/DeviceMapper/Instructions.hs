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

data Instruction =
    Suspend DeviceName |
    Resume DeviceName |
    Load DeviceName Text |
    Create DeviceName |
    Remove DeviceName
    deriving (Show, Eq)

prettyInstruction :: Instruction -> Doc ()
prettyInstruction (Suspend n) = pretty "suspend" <+> pretty n
prettyInstruction (Resume n) = pretty "resume" <+> pretty n
prettyInstruction (Load n txt) = hsep [
    pretty "load",
    pretty n,
    hardline,
    pretty "    " <> (align $ pretty txt)
    ]
prettyInstruction (Create n) = hsep . map pretty $ [T.pack "create", n]
prettyInstruction (Remove n) = pretty "remove" <+> pretty n

-- FIXME: use a proper pretty printer
prettyProgram :: [Instruction] -> Doc ()
prettyProgram = vcat . map prettyInstruction

