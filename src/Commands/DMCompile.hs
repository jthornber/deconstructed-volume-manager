{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Commands.DMCompile (
    dmCompileCmd,

    -- Exported for testing
    uniq
    ) where

import Protolude

import qualified DeviceMapper.Instructions as I
import DeviceMapper.LowLevelTypes
import DeviceMapper.HighLevelTypes

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map as M
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

----------------------------------------------

join' :: [Text] -> Text
join' = T.intercalate " "

formatError :: ErrorTarget -> TableLine
formatError (ErrorTarget len) = TableLine "error" len ""

formatLinear :: LinearTarget -> TableLine
formatLinear (LinearTarget dev b e) =
    TableLine "linear" (e - b) (T.concat [ devPath dev, " ", T.pack $ show b])

formatStriped :: StripedTarget -> TableLine
formatStriped (StripedTarget l c ds) =
    TableLine "striped" l (join' ([T.pack (show c)] ++ concatMap expand ds))
    where
        expand (DeviceOffset d s) = [devPath d, T.pack $ show s]

formatThinPool :: ThinPoolTarget -> TableLine
formatThinPool tp = TableLine "thin-pool" (thinPoolLen tp) args
    where
        args = join' ([
                dev thinPoolMetadataDev,
                dev thinPoolDataDev,
                nr thinPoolBlockSize,
                nr thinPoolLowWaterMark,
                len opts] ++ opts)

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

        nr fn = T.pack . show . fn $ tp
        dev fn = devPath . fn $ tp
        len = T.pack . show . length

formatThin :: ThinTarget -> TableLine
formatThin t = TableLine "thin" (thinLen t) args
    where
        args = join' ([
            dev thinPoolDev,
            nr thinId] ++ (maybeToList (devPath <$> (thinExternalOrigin t))))
        nr fn = T.pack . show . fn $ t
        dev fn = devPath . fn $ t

formatCache :: CacheTarget -> TableLine
formatCache c = TableLine "cache" (cacheLen c) args
    where
        args = join' [
            dev cacheMetadataDev,
            dev cacheFastDev,
            dev cacheOriginDev,
            nr cacheBlockSize,
            len (cacheFeatures c),
            join' (cacheFeatures c),
            formatPolicyLine (cachePolicy c)]
        nr fn = T.pack . show . fn $ c
        dev fn = devPath . fn $ c
        len = T.pack . show . length
        formatPolicyLine (CachePolicy n ks) = join' ([n] ++ (concatMap expand ks))
        expand (k, v) = [k, v]

toTableLine :: Target -> TableLine
toTableLine (ErrorType v) = formatError v
toTableLine (LinearType v) = formatLinear v
toTableLine (StripedType v) = formatStriped v
toTableLine (ThinPoolType v) = formatThinPool v
toTableLine (ThinType v) = formatThin v
toTableLine (CacheType v) = formatCache v

-- FIXME: rewrite the format functions to use Text.Builder


----------------------------------------------
-- Target device dependencies

depsStriped (StripedTarget l c ds) = map (\(DeviceOffset d _) -> d) ds
    where
        expand (DeviceOffset d s) = [devPath d, T.pack $ show s]

depsThinPool tp = [thinPoolDataDev tp, thinPoolMetadataDev tp]
depsThin t = [thinPoolDev t] ++ (maybeToList . thinExternalOrigin $ t)
depsCache c = [ cacheMetadataDev c, cacheFastDev c, cacheOriginDev c]

getTargetDeps :: Target -> [Device]
getTargetDeps (ErrorType _) = []
getTargetDeps (LinearType (LinearTarget d _ _)) = [d]
getTargetDeps (StripedType v) = depsStriped v
getTargetDeps (ThinPoolType v) = depsThinPool v
getTargetDeps (ThinType v) = depsThin v
getTargetDeps (CacheType v) = depsCache v

devToId :: Device -> Maybe DeviceId
devToId (ExternalDevice _) = Nothing
devToId (DMDevice d) = Just d

getTargetDMDeps :: Target -> [DeviceId]
getTargetDMDeps = catMaybes . map devToId . getTargetDeps

----------------------------------------------
-- Activation ordering

-- Filters a list such that each element only appears once,
-- and that occurence is the first in the list.
-- eg, [1, 3, 1, 5, 5] -> [1, 3, 5]
uniq :: (Ord a) => [a] -> [a]
uniq = reverse . fst . foldr go ([], Set.empty) . reverse
    where
        go :: (Ord a) => a -> ([a], Set.Set a) -> ([a], Set.Set a)
        go x (xs, seen)
            | Set.member x seen = (xs, seen)
            | otherwise         = (x:xs, Set.insert x seen)

shallowDeps :: TableMap -> DeviceId -> [DeviceId]
shallowDeps tm d = case M.lookup d tm of
    Just ts -> concatMap getTargetDMDeps $ ts
    Nothing -> []

-- FIXME: detect circular deps
-- I'm assuming there are no circular deps for now
deepDeps :: TableMap -> DeviceId -> [DeviceId]
deepDeps tm = concatMap (deepDeps tm) . shallowDeps tm

-- Given a list of top level devices give back the order
-- we need to activate the devs.
-- Assumes no cyclic deps.
sortByActivation :: TableMap -> [DeviceId] -> [DeviceId]
sortByActivation tm = uniq . concatMap (deepDeps tm)

----------------------------------------------
-- TableMap -> IR

data IR = Create DeviceId |
          Remove DeviceId |
          Load DeviceId [TableLine] |
          Suspend DeviceId |
          Resume DeviceId |
          List |
          Test IR IR |
          Noop |
          Fail Int |
          Begin [IR]
          deriving (Eq, Show)

type TableMap = Map DeviceId [Target]

-- Does not unpick.
deactivateMany :: [DeviceId] -> IR
deactivateMany [] = Noop
deactivateMany (dev:rest) = Begin [
    Remove dev,
    deactivateMany rest]

-- Assumes the devices have been sorted into activation order.
-- Unpicks.
activateMany :: TableMap -> [DeviceId] -> [DeviceId] -> Maybe IR
activateMany _ [] _ = Just Noop
activateMany tm (dev : rest) unpick = do
    table <- (map toTableLine) <$> M.lookup dev tm
    activateRestIR <- activateMany tm rest (dev:unpick)
    let unpickBadCreateIR = deactivateMany unpick
    let unpickBadLoadIR = deactivateMany (dev:unpick)
    pure $
        Begin [Create dev,
               Test (Begin [Load dev table,
                            Test activateRestIR
                                 unpickBadLoadIR])
                    unpickBadCreateIR]

activate :: TableMap -> Maybe IR
activate tm = activateMany tm devs []
    where
        devs = M.foldrWithKey (\k _ -> (k :)) [] tm

----------------------------------------------
-- IR -> I.Program

-- A little state monad to manage the labels
type Labeller = State Int

mkLabel :: Labeller Text
mkLabel = do
    n <- get
    put (n + 1)
    pure (T.pack . show $ n)

wrap :: a -> Seq a
wrap = S.singleton

-- FIXME: missing cases
linearise :: IR -> Labeller (Seq I.Instruction)
linearise (Create dev) = pure . wrap $ I.Create dev
linearise (Remove dev) = pure . wrap $ I.Remove dev
linearise (Load dev table) = pure . wrap $ I.Load dev table
linearise (Suspend dev) = pure . wrap $ I.Suspend dev
linearise (Resume dev) = pure . wrap $ I.Remove dev
linearise (Test good bad) = do
    bad_label <- mkLabel
    out_label <- mkLabel
    good_code <- linearise good
    bad_code <- linearise bad
    pure $
        wrap (I.JmpFail bad_label) ><
        good_code ><
        wrap (I.Jmp out_label) ><
        wrap (I.Label bad_label) ><
        bad_code ><
        wrap (I.Label out_label)
linearise (Noop) = pure S.empty
linearise (Fail n) = pure . wrap $ I.Exit n
linearise (Begin irs) = do
    codes <- mapM linearise irs
    pure $ foldr (><) S.empty codes

-- We can get multiple labels at the same point in the code because
-- of empty success branches.
tidyLabels :: Seq I.Instruction -> Seq I.Instruction
tidyLabels instrs = S.fromList tidied
    where
        lst = toList instrs

        aliases :: Map Text Text
        aliases = foldr insertAliases M.empty chunks

        insertAliases :: [I.Instruction] -> Map Text Text -> Map Text Text
        insertAliases [] z = undefined -- "empty chunk"
        insertAliases [x] z = z
        insertAliases ((I.Label v):rst) z = foldr (\(I.Label k) -> M.insert k v) z rst
        insertAliases _ _ = undefined -- "not a label"

        isLabel :: I.Instruction -> Bool
        isLabel (I.Label _) = True
        isLabel _ = False

        chunks = unfoldr getChunk lst
        getChunk [] = Nothing
        getChunk xs = Just . L.span isLabel . dropWhile (not . isLabel) $ xs

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
toProgram ir = I.mkProgram (toList $ tidyLabels (evalState (linearise ir) 0))

--------------------------------------------

pError :: Text -> IO ()
pError = hPutStrLn stderr

usage :: IO ExitCode
usage = do
    pError "usage: dm-compile <device description file>"
    pure $ ExitFailure 1

readDevices :: Text -> IO (Either Text TableMap)
readDevices path = do
    contents <- L8.readFile $ T.unpack path
    case eitherDecode contents of
        Left err -> pure $ Left $ T.pack err
        Right xs -> pure . Right . M.fromList $ xs

-- FIXME: use EitherT a b IO ?
dmCompileCmd :: [Text] -> IO ExitCode
dmCompileCmd [path] = do
    edevs <- readDevices path
    case edevs of
        Left err -> do
            pError "Invalid device description: "
            pError err
            pure $ ExitFailure 1
        Right devs -> do
            case activate devs of
                Nothing -> do
                    pError "Compile failed.  Recursive devs?"
                    pure (ExitFailure 1)
                Just ir -> do
                    L8.putStrLn . encodePretty . toProgram $ ir
                    pure ExitSuccess
dmCompileCmd _ = usage

