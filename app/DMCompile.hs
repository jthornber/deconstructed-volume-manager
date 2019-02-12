{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module DMCompile (
    dmCompileCmd
    ) where

import Control.Monad.State
import qualified DeviceMapper.Instructions as I
import DeviceMapper.Types
import DeviceMapper.Targets

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

----------------------------------------------

-- Compilation steps:
-- i) read before and after device descriptions
-- ii) compile to an intermediate representation (tree like)
-- iii) flatten to dmexec
-- iv) output

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
--

-- Change dmexec semantics so it will not do anything until the IoctlFailed
-- flag is cleared.  So we can just fall through?

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
allDepDevs d@(DMDevice n t) = (concatMap allDepDevs . tableDeps $ t) ++ [(n, t)]
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

sda, sdb :: Device
sda = ExternalDevice (T.pack "/dev/sda")
sdb = ExternalDevice (T.pack "/dev/sdb")

dmCompileCmd :: [Text] -> IO ExitCode
dmCompileCmd _ = do
    L8.putStrLn . encodePretty $ program
    return ExitSuccess

    where
        dev = DMDevice (DeviceId {devName = (T.pack "test1"), devUUID = Nothing}) table
        table = Table [toTarget (Linear sda 0 1024),
                       toTarget (Linear sdb 0 1024)]
        program = toProgram . activate $ dev

