{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module DMCompile (
    dmCompileCmd
    ) where

import Control.Monad.State
import qualified DeviceMapper.Instructions as I
import DeviceMapper.LowLevelTypes
import DeviceMapper.HighLevelTypes

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq, (><), (|>), (<|))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util
import GHC.Generics
import System.Exit
import System.IO

----------------------------------------------

data LowLevelTarget = LowLevelTarget {
    targetLine :: TableLine,
    targetDeps :: [Device]
} deriving (Eq, Show)

class ToTarget a where
    toTarget :: a -> LowLevelTarget

instance ToTarget ErrorTarget where
    toTarget (ErrorTarget len) = LowLevelTarget {
        targetLine = TableLine "error" len "",
        targetDeps = []
    }

instance ToTarget LinearTarget where
    toTarget (LinearTarget dev b e) = LowLevelTarget {
        targetLine = TableLine "linear" (e - b) (T.concat [
            devPath dev,
            " ",
            T.pack $ show b]),
        targetDeps = [dev]
    }

instance ToTarget StripedTarget where
    toTarget (StripedTarget l c ds) = LowLevelTarget {
        targetLine = TableLine "striped" l (join ([T.pack (show c)] ++
                                         concatMap expand ds)),
        targetDeps = map (\(DeviceOffset d _) -> d) ds
    }
        where
            expand (DeviceOffset d s) = [devPath d, T.pack $ show s]
            join = T.intercalate (T.pack " ")

formatTPLine :: ThinPoolTarget -> Text
formatTPLine tp = join ([
    dev thinPoolMetadataDev,
    dev thinPoolDataDev,
    nr thinPoolBlockSize,
    nr thinPoolLowWaterMark,
    len opts] ++ opts)
    where
        opts = concatMap maybeToList [
            rflag thinPoolZero "skip-block-zeroing",
            rflag thinPoolDiscard "ignore-discard",
            flag thinPoolDiscardPassdown "no-discard-passdown",
            flag thinPoolReadOnly "read-only",
            flag thinPoolErrorIfNoSpace "error-if-no-space"]

        flag fn f = if fn tp
                    then Just . T.pack $ f
                    else Nothing

        rflag fn = flag (not . fn)

        lit v = T.pack v
        nr fn = T.pack . show . fn $ tp
        dev fn = devPath . fn $ tp
        len = T.pack . show . length
        join = T.intercalate (T.pack " ")

instance ToTarget ThinPoolTarget where
    toTarget tp = LowLevelTarget {
        targetLine = TableLine "thin-pool" (thinPoolLen tp) (formatTPLine tp),
        targetDeps = [thinPoolDataDev tp, thinPoolMetadataDev tp]
    }

formatTLine :: ThinTarget -> Text
formatTLine t = join ([
    dev thinPoolDev,
    nr thinId] ++ (maybeToList (devPath <$> (thinExternalOrigin t))))
    where
        lit v = T.pack v
        nr fn = T.pack . show . fn $ t
        dev fn = devPath . fn $ t
        join = T.intercalate (T.pack " ")

instance ToTarget ThinTarget where
    toTarget t = LowLevelTarget {
        targetLine = TableLine "thin" (thinLen t) (formatTLine t),
        targetDeps = [thinPoolDev t] ++ (maybeToList . thinExternalOrigin $ t)
    }

formatCLine :: CacheTarget -> Text
formatCLine c = join [
    dev cacheMetadataDev,
    dev cacheFastDev,
    dev cacheOriginDev,
    nr cacheBlockSize,
    len (cacheFeatures c),
    join (cacheFeatures c),
    formatPolicyLine (cachePolicy c)]
    where
        lit v = T.pack v
        nr fn = T.pack . show . fn $ c
        dev fn = devPath . fn $ c
        len = T.pack . show . length
        join = T.intercalate (T.pack " ")
        formatPolicyLine (CachePolicy n ks) = join ([n] ++ (concatMap expand ks))
        expand (k, v) = [k, v]

instance ToTarget CacheTarget where
    toTarget c = LowLevelTarget {
        targetLine = TableLine "cache" (cacheLen c) (formatCLine c),
        targetDeps = [
            cacheMetadataDev c,
            cacheFastDev c,
            cacheOriginDev c]
    }

newtype Table = Table {
    tableTargets :: [LowLevelTarget]
} deriving (Eq, Show)

tableDeps :: Table -> [Device]
tableDeps = concatMap targetDeps . tableTargets

tablePrepare :: Table -> [TableLine]
tablePrepare = map targetLine . tableTargets

{-
----------------------------------------------
-- Table Map

type TableMap = Map DeviceId [TargetP]

----------------------------------------------
-- Compilation steps:
--   read before and after device descriptions
--   compile to an intermediate representation (tree like)
--   flatten to dm-exec instructions
--   output

----------------------------------------------
-- Intermediate rep

data IR = Create DeviceId |
          Remove DeviceId |
          Load DeviceId Table |
          Suspend DeviceId |
          Resume DeviceId |
          List |
          Test IR IR |
          Noop |
          Fail Int |
          Begin [IR]
          deriving (Eq, Show)

-- When executing a program, there are the following outcomes:
-- 1) success
-- 2) fail, but managed to reverse, so all as at start
-- 3) fail, couldn't unpick
-- Can we just make do with 3 exit status's then?  Does the
-- program above need to know exactly what failed?  eg, corrupt
-- thin metadata leading to a thin_check/repair?  So programs need
-- to be using the error codes to cover a large number of potential failure cases.
-- Information to interpret these cases will have to be handed back
-- along with the compiled program.  They can't just be a text string, because
-- we have to act upon them.
-- eg,
-- err 4 - Activate myThinPool
-- err 5 - Resume myThinPool
-- err 99 - ThinCreateMessage myThinPool 4

activateDevs :: [(DeviceId, Table)] -> IR
activateDevs devs = activate' devs []
    where
        deactivate' [] = Noop
        deactivate' (dev:rest) = Begin [Remove dev, deactivate' rest]

        activate' [] _ = Noop
        activate' ((dev, table):rest) unpick =
            Begin [Create dev,
                   Test (Begin [Load dev table,
                                Test (activate' rest (dev:unpick))
                                     (deactivate' (dev:unpick))])
                        (deactivate' unpick)]

-- Returns the devices sorted bottom up
allDepDevs :: Device -> [(DeviceId, Table)]
allDepDevs d@(DMDevice n) = (concatMap allDepDevs . tableDeps $ t) ++ [(n, t)]
allDepDevs _ = []

activate :: Device -> IR
activate d = activateDevs . toList $ devs
    where
        devs = allDepDevs d

----------------------------------------------

-- A little state monad to manage the labels
type Labeller = State Int

mkLabel :: Labeller Text
mkLabel = do
    n <- get
    put (n + 1)
    return (T.pack . show $ n)

s = S.singleton

linearise :: IR -> Labeller (Seq I.Instruction)
linearise (Create dev) = return . s $ I.Create dev
linearise (Remove dev) = return . s $ I.Remove dev
linearise (Load dev table) = return . s $ I.Load dev (tablePrepare table)
linearise (Suspend dev) = return . s $ I.Suspend dev
linearise (Resume dev) = return . s $ I.Remove dev
linearise (Test good bad) = do
    bad_label <- mkLabel
    out_label <- mkLabel
    good_code <- linearise good
    bad_code <- linearise bad
    return $
        s (I.JmpFail bad_label) ><
        good_code ><
        s (I.Jmp out_label) ><
        s (I.Label bad_label) ><
        bad_code ><
        s (I.Label out_label)
linearise (Noop) = return S.empty
linearise (Fail n) = return . s $ I.Exit n
linearise (Begin irs) = do
    codes <- mapM linearise irs
    return $ foldr (><) S.empty codes

-- We can get multiple labels at the same point in the code because
-- of empty success branches.
tidyLabels :: Seq I.Instruction -> Seq I.Instruction
tidyLabels instrs = S.fromList tidied
    where
        lst = toList instrs

        aliases :: Map Text Text
        aliases = foldr insertAliases M.empty chunks

        insertAliases :: [I.Instruction] -> Map Text Text -> Map Text Text
        insertAliases [] z = error "empty chunk"
        insertAliases [x] z = z
        insertAliases ((I.Label v):rst) z = foldr (\(I.Label k) -> M.insert k v) z rst
        insertAliases _ _ = error "not a label"

        isLabel :: I.Instruction -> Bool
        isLabel (I.Label _) = True
        isLabel _ = False

        chunks = unfoldr getChunk lst
        getChunk [] = Nothing
        getChunk xs = Just . span isLabel . dropWhile (not . isLabel) $ xs

        tidied = concatMap tidy lst

        tidy :: I.Instruction -> [I.Instruction]
        tidy i@(I.Label l) =
            case M.lookup l aliases of
                Nothing -> [i]
                Just _ -> []
        tidy i@(I.Jmp l) =
            case M.lookup l aliases of
                Nothing -> [i]
                Just l2 -> [I.Jmp l2]
        tidy i@(I.JmpFail l) =
            case M.lookup l aliases of
                Nothing -> [i]
                Just l2 -> [I.JmpFail l2]
        tidy i = [i]

-- FIXME: we can also simplify a sequence of Jmps


toProgram :: IR -> I.Program
toProgram ir = I.mkProgram 0 (toList $ tidyLabels (evalState (linearise ir) 0))

--------------------------------------------

usage :: IO ExitCode
usage = do
    hPutStrLn stderr "usage: dm-compile <device description file>"
    return $ ExitFailure 1

readDevices :: FilePath -> IO (Either String [(Device, [TargetP])])
readDevices path = L8.readFile path >>= (return . eitherDecode)

dmCompileCmd :: [Text] -> IO ExitCode
dmCompileCmd args = do
    if length args /= 1
    then usage
    else do
        edevs <- readDevices . T.unpack . head $ args
        case edevs of
            Left err -> do
                hPutStr stderr "Invalid device description: "
                hPutStrLn stderr err
                return $ ExitFailure 1
            Right devs -> do
                toProgram . activate $ devs
-}

dmCompileCmd :: [Text] -> IO ExitCode
dmCompileCmd args = do
    putStrLn "not implemented"
    return ExitSuccess
