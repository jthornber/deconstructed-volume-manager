
module Commands.DMExec (
        dmExecCmd
    ) where

import Protolude

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as LS
import qualified DeviceMapper.VM as VM

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Formats.DMExec

-------------------------------------------

pError :: Text -> IO ()
pError = pError

usage :: IO ExitCode
usage = do
    pError "usage: dm-exec <program file>"
    pure $ ExitFailure 1

readProgram :: Text -> IO (Either Text VM.Program)
readProgram path = parseAsm <$> T.readFile (T.unpack path)

dmExecCmd :: [Text] -> IO ExitCode
dmExecCmd [path] = do
    eprg <- readProgram path
    case eprg of
        Left err -> do
            pError "Invalid program: "
            pError err
            pure $ ExitFailure 1
        Right prg -> do
            (exitCode, obj) <- VM.runVM prg
            LS.putStrLn . encodePretty $ obj
            if exitCode == 0
            then pure ExitSuccess
            else pure $ ExitFailure exitCode
dmExecCmd _ = usage

-------------------------------------------
