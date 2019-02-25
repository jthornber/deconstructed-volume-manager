module Main where

import Commands.DMCompile
import Commands.DMExec

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Environment
import System.Exit
import System.IO

commands :: M.Map String ([T.Text] -> IO ExitCode)
commands = M.fromList [
    ("dm-exec", dmExecCmd),
    ("dm-compile", dmCompileCmd)]

usage :: IO ()
usage = do
    hPutStrLn stderr "<helpful usage info>"
    exitWith (ExitFailure 1)

dispatch :: String -> [String] -> IO ()
dispatch cmd args =
    case M.lookup cmd commands of
        Nothing -> do
            hPutStrLn stderr "No such command"
            usage
        Just fn -> do
            e <- fn (map T.pack args)
            exitWith e

main :: IO ()
main = do
    args <- getArgs
    case args of
        cmd:rest -> dispatch cmd rest
        _ -> usage


