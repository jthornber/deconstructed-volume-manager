{-# LANGUAGE DeriveGeneric #-}

module Replicate (
    replicateCmd
    ) where

import Protolude
import Data.Aeson

-- Regions need to be passed in as a chunk of JSON data

newtype Page = Page Integer deriving (Generic, Show)

instance ToJSON Page
instance FromJSON Page

--------------------------------------
-- Regions

{-
data Region = Region {
    regionPath :: FilePath,
    regionBegin :: Page,
    regionEnd :: Page
} deriving (Generic, Show)

instance ToJSON Region where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Region
    -- No need to provide a parseJSON implementation

-- Need to review basic file IO.
-- fseek, O_DIRECT, bytestrings, flushing

readRegions :: FilePath -> IO (Maybe [Region])
readRegions path = do
    txt <- L8.readFile path
    pure (decode txt)

showRegions :: FilePath -> IO ()
showRegions path = readRegions path >>= L8.putStrLn . encode

--------------------------------------
-- Verify

{-
verifyRegions :: [Region] -> IO (Either [String] ())
verifyRegions rs = do
    hashes <- mapConcurrently hashRegion rs
    if all (== (head hashes)) (tail hashes)
    then pure $ Right ()
    else pure $ Left ["Hashes differ"]
    -}
verifyRegions = undefined

--------------------------------------
-- Top level
fail' :: IO ()
fail' = exitWith (ExitFailure 1)

barf :: Text -> IO ()
barf msg = putStrLn msg >> fail'

usage :: IO ()
usage = do
    putStrLn ("usage: replicate <region file>" :: Text)
    exitWith (ExitFailure 1)

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
    then usage
    else do
        regions <- readRegions (head args)
        case regions of
            Nothing -> barf "Couldn't read region file"
            Just regions -> do
                result <- verifyRegions regions
                case result of
                    Left msgs -> mapM_ putStrLn msgs >> fail'
                    Right () -> putStrLn "Verified!"
                    -}

replicateCmd :: [Text] -> IO ()
replicateCmd _ = putStrLn ("Hello, world!" :: Text)

