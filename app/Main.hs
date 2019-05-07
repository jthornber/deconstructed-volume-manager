module Main where
        
import Commands.DMCompile
import Commands.DMExec

import Protolude
import qualified Data.Map.Strict as M
import qualified Data.Text as T

commands :: M.Map Text ([T.Text] -> IO ExitCode)
commands = M.fromList [
    ("dm-exec", dmExecCmd),
    ("dm-compile", dmCompileCmd)]

pError :: Text -> IO ()
pError = hPutStrLn stderr

usage :: IO ()
usage = do
    pError "<helpful usage info>"
    exitWith (ExitFailure 1)

dispatch :: Text -> [Text] -> IO ()
dispatch cmd args =
    case M.lookup cmd commands of
        Nothing -> do
            pError "No such command"
            usage
        Just fn -> do
            e <- fn args
            exitWith e

getTextArgs :: IO [Text]
getTextArgs = (map T.pack) <$> getArgs

main :: IO ()
main = do
    args <- getTextArgs
    case args of
        cmd:rest -> dispatch cmd rest
        _ -> usage


