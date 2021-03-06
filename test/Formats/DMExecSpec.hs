module Formats.DMExecSpec (
        spec
    ) where

import Protolude

import Data.Attoparsec.Text
import Data.Either
import Data.Map.Strict as M
import Data.Text (Text)
import Data.Text.Encoding as E
import Data.Text as T
import Data.Text.IO as T
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import DeviceMapper.LowLevelTypes
import qualified DeviceMapper.VM as VM
import Formats.DMExec
import Data.List

parseSingleChunk p input = feed (parse p input) ""

parseGood :: (Show a, Eq a) => Parser a -> Text -> a -> IO ()
parseGood p input output = eitherResult (parseSingleChunk p input) `shouldBe` (Right output)

parseGood_ p input = parseGood p input ()

parseBad :: (Show a, Eq a) => Parser a -> Text -> IO ()
parseBad p input = eitherResult (parseSingleChunk p input) `shouldSatisfy` isLeft

emptyDecls = M.fromList []
decls1 = M.fromList [("foo", Dev (DeviceId "foobar" Nothing))]
decls2 = M.fromList [
    ("foo", Dev (DeviceId "foo" Nothing)),
    ("table", Table [TableLine "linear" 1024 "/dev/sdc 0"])]

ex :: Int -> VM.Program -> IO ()
ex n expected = do
    txt <- T.readFile path
    parseGood program txt expected
    where
        path = "./examples/dm-exec/ex" ++ (show n) ++ ".asm"

spec = parallel $ do
    describe "Formats.DMExec.parser" $ do
        describe "space" $ do
            it "should accept an empty string" $
                parseGood space' "" ()
            it "should accept a single space" $
                parseGood space' " " ()
            it "should accept multiple spaces" $
                parseGood space' "  \t  \t\n\n   " ()

        describe "tok" $ do
            it "should handle no spaces" $
                parseGood_ (tok $ lit "foo") "foo"
            it "should handle leading spaces" $
                parseGood_ (tok $ lit "foo") "   \t\t\n  foo"

        describe "lit" $ do
            it "should match strings" $
                parseGood_ (lit "foo") "foo"
            it "should fail if no match" $
                parseBad (lit "foo") "bar"

        describe "identifer" $ do
            it "should not start with a digit" $
                parseBad identifier "123"
            it "can have a single char" $
                parseGood identifier "x" "x"
            it "can have many characters" $
                parseGood identifier "foobar" "foobar"
            it "can have digits in the tail" $
                parseGood identifier "foo23bar" "foo23bar"
            it "ignores leading space" $
                parseGood identifier "   foo" "foo"
            it "stop on space" $
                parseGood identifier "foo  bar" "foo"

        describe "quotedString" $ do
            it "should accept an empty string" $
                parseGood quotedString "\"\"" ""
            it "should accept a simple string" $
                parseGood quotedString "\"foo\"" "foo"
            it "should skip leading space" $
                parseGood quotedString "   \"foo\"  " "foo"
            it "should read embedded space" $
                parseGood quotedString "\"foo bar\"" "foo bar"
            it "should handle tabs" $
                parseGood quotedString "\"foo\\tbar\"" "foo\tbar"
            it "should handle newlines" $
                parseGood quotedString "\"foo\\nbar\"" "foo\nbar"
            it "should handle quoted backslash" $
                parseGood quotedString "\"foo\\\\bar\"" "foo\\bar"


        describe "deviceDecl" $ do
            it "should accept just a name" $
                parseGood deviceDecl "device metadata = {name = \"test-lv\"}"
                    ("metadata", (Dev (DeviceId "test-lv" Nothing)))
            it "should accept a name and a uuid" $
                parseGood deviceDecl "device metadata = { name=\"lv1\" , uuid = \"asdf\" }"
                    ("metadata", (Dev (DeviceId "lv1" (Just "asdf"))))

        describe "tableDecl" $ do
            it "should accept an empty table" $
                parseGood tableDecl "table t = []" ("t", Table [])
            it "should accept a single line" $
                parseGood tableDecl "table t = [\n  linear 1024 \t\"/dev/sdc 0\"\n]"
                    ("t", Table [TableLine "linear" 1024 "/dev/sdc 0"])
            it "should accept multiple targets" $
                parseGood tableDecl "table t = [linear 1024 \"/dev/sdc 0\"  ,\nerror 1024 \"\"]"
                    ("t", Table [TableLine "linear" 1024 "/dev/sdc 0",
                                 TableLine "error" 1024 ""])

        describe "decls" $ do
            it "should accept empty decls" $
                parseGood decls "" (M.fromList [])

        describe "beginning of line" $ do
            it "should fail if there's leading space" $
                parseBad (bol foo) " foo"

            it "should accept with no trailing space" $
                parseGood_ (bol foo) "foo"

            it "should accept with trailing space" $
                parseGood_ (bol foo) "foo \t"

            it "should accept with newline" $
                parseGood_ (bol foo) "foo\n"

            it "should accept with trailing space and newline" $
                parseGood_ (bol foo) "foo  \t  \n"

        describe "label" $ do
            it "should accept labels at the start of the line" $
                parseGood (bol labelInstr) ".start" (Label "start")
            it "should reject labels that don't start with a dot" $
                parseBad (bol labelInstr) "start"
            it "should reject labels with zero length names" $
                parseBad (bol labelInstr) ".\n"
            it "should reject labels with leading space" $
                parseBad (bol labelInstr) "   .start"

        describe "instruction" $ do
            it "should reject an unknown instruction" $
                parseBad (instruction emptyDecls) "release-monkeys"

            it "should accept remove-all" $
                parseGood (instruction emptyDecls) "remove-all" RemoveAll

            it "should accept list" $
                parseGood (instruction emptyDecls) "list \"foo\"" (List "foo")

            it "should accept create" $
                parseGood (instruction decls1) "create foo" (Create (DeviceId "foobar" Nothing))

            it "should accept remove" $
                parseGood (instruction decls1) "remove foo" (Remove (DeviceId "foobar" Nothing))

            it "should accept suspend" $
                parseGood (instruction decls1) "suspend foo" (Suspend (DeviceId "foobar" Nothing))

            it "should accept resume" $
                parseGood (instruction decls1) "resume foo" (Resume (DeviceId "foobar" Nothing))

            it "should accept load" $
                parseGood (instruction decls2) "load foo table" $
                    Load (DeviceId "foo" Nothing) [TableLine "linear" 1024 "/dev/sdc 0"]

            it "should accept info" $
                parseGood (instruction decls1) "info \"clang\" foo"
                    (InfoQ "clang" (DeviceId "foobar" Nothing))


            it "should accept table" $
                parseGood (instruction decls1) "table \"clang\" foo"
                    (TableQ "clang" (DeviceId "foobar" Nothing))

            it "should accept begin" $
                parseGood (instruction emptyDecls) "begin" BeginObject

            it "should accept end" $
                parseGood (instruction emptyDecls) "end \"kelp\"" (EndObject "kelp")

            it "should accept literals" $
                parseGood (instruction emptyDecls) "literal \"empire\" \"dragon\"" $
                    Literal "empire" "dragon"

            it "should accept jmp" $
                parseGood (instruction emptyDecls) "jmp high" $ Jmp "high"

            it "should accept jmp-fail" $
                parseGood (instruction emptyDecls) "jmp-fail high" $ JmpFail "high"

            it "should accept exit" $
                parseGood (instruction emptyDecls) "exit 234" $ Exit 234

        describe "labelOrInstr" $ do
            it "should handle a label" $
                parseGood (labelOrInstr emptyDecls) ".start" $ Label "start"
            it "should handle an instruction" $
                parseGood (labelOrInstr emptyDecls) "  list \"foo\"" $ List "foo"
            it "should work with sepBy" $
                parseGood (sepBy (labelOrInstr emptyDecls) endLine)
                    ".start\n  list \"foo\""
                    [Label "start", List "foo"]

        describe "program" $ do
            it "should handle empty programs" $ ex 1 (VM.mkProgram [])
            it "should handle just declarations" $ ex 2 (VM.mkProgram [])
            it "should handle no declarations" $ ex 3 (VM.mkProgram [VM.List "foo"])
            it "should handle labels" $ ex 4 (VM.mkProgram
                [VM.List "devs",
                 VM.JmpFail 0,
                 VM.Create (DeviceId {devName = "foo", devUUID = Nothing}),
                 VM.Jmp 2,
                 VM.Remove (DeviceId {devName = "foo", devUUID = Nothing}),
                 VM.Jmp 0])

