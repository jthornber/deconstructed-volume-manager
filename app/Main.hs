module Main where

import Lib
import Activate
import DMExec

import qualified Data.Text as T
import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    e <- dmExecCmd (map T.pack args)
    exitWith e
