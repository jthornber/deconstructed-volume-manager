module Formats.DMExec (
    Decl (..),
    parseAsm,

    -- only exported for testing
    Declarations,
    AsmInstruction (..),
    lit,
    space',
    tok,
    identifier,
    quotedString,
    deviceDecl,
    tableDecl,
    decls,
    instruction,
    bol,
    labelInstr,
    labelOrInstr,
    indented,
    endLine,
    foo,
    program
    ) where

import Protolude

import Control.Monad
import Data.Attoparsec.Text
import Data.Char
import Data.Map.Strict as M
import Data.Text (Text)
import Data.Text as T
import qualified DeviceMapper.Instructions as I
import DeviceMapper.LowLevelTypes

-- Declarations are used to predefine literals to make the instructions
-- more succinct

data Decl =
    Dev DeviceId |
    Table [TableLine]
    deriving (Eq, Show)

type Declarations = M.Map Text Decl

data AsmInstruction =
    RemoveAll |
    List Text |
    Create DeviceId |
    Remove DeviceId |
    Suspend DeviceId |
    Resume DeviceId |
    Load DeviceId [TableLine] |
    InfoQ Text DeviceId |
    TableQ Text DeviceId |
    BeginObject |
    EndObject Text |
    Literal Text Text |
    Jmp Text |
    JmpFail Text |
    Label Text |
    Exit Int
    deriving (Show, Eq)

around :: Parser a -> Parser b -> Parser c -> Parser c
around open close p = open *> p <* close

space' :: Parser ()
space' = many space >> pure ()

-- FIXME: handle comments from within tok
tok :: Parser a -> Parser a
tok p = space' *> p

lit :: Text -> Parser ()
lit txt = tok (string txt) *> pure ()

-- A trivial parser used in tests
foo :: Parser ()
foo = string "foo" *> pure ()

identifierRaw :: Parser Text
identifierRaw = do
    c <- satisfy isAlpha
    cs <- Data.Attoparsec.Text.takeWhile idChar
    pure $ T.cons c cs
    where
        idChar c = isDigit c || isAlpha c

identifier :: Parser Text
identifier = tok identifierRaw

doubleQuote :: Parser ()
doubleQuote = char '"' *> pure ()

escapedChar :: Parser Char
escapedChar = do
    c <- notChar '"'
    case c of
        '\\' -> do
            c' <- anyChar
            case c' of
                't' -> pure '\t'
                'n' -> pure '\n'
                _ -> pure c'
        _ -> pure c

quotedString :: Parser Text
quotedString = tok (doubleQuote *> (T.pack <$> (many' escapedChar)) <* doubleQuote)

reqPair :: Text -> Parser Text
reqPair k = do
    k' <- identifier
    guard (k' == k)
    lit "="
    quotedString

deviceDecl :: Parser (Text, Decl)
deviceDecl = do
    lit "device"
    var <- identifier
    lit "="
    lit "{"
    name <- reqPair "name"
    uuid <- (Just <$> (lit "," *> reqPair "uuid")) <|> pure Nothing
    lit "}"
    pure (var, Dev (DeviceId name uuid))

sectors :: Parser Integer
sectors = tok decimal

targetType :: Parser Text
targetType = identifier

tableLine :: Parser TableLine
tableLine = TableLine <$> targetType <*> sectors <*> quotedString

table :: Parser Decl
table = Table <$> around (lit "[") (lit "]") (sepBy tableLine (tok $ char ','))

tableDecl :: Parser (Text, Decl)
tableDecl = do
    lit "table"
    var <- identifier
    lit "="
    d <- table
    pure (var, d)

decl :: Parser (Text, Decl)
decl = do
    _ <- many blankLine
    deviceDecl <|> tableDecl

decls :: Parser Declarations
decls = M.fromList <$> many' decl

labelInstr :: Parser AsmInstruction
labelInstr = char '.' *> (Label <$> identifierRaw)

key :: Parser Text
key = quotedString

value :: Parser Text
value = quotedString

deviceRef :: Declarations -> Parser DeviceId
deviceRef ds = do
    var <- identifier
    case M.lookup var ds of
       (Just (Dev d)) -> pure d
       (Just _) -> fail "expected DeviceId, but got Table"
       _ -> fail "no such DeviceId"

tableRef :: Declarations -> Parser [TableLine]
tableRef ds = do
    var <- identifier
    case M.lookup var ds of
       (Just (Table t)) -> pure t
       (Just _) -> fail "expected Table, but got DeviceId"
       _ -> fail "no such Table"

labelRef :: Parser Text
labelRef = identifier

nonBreakSpace :: Parser ()
nonBreakSpace = (char ' ' <|> char '\t') >> pure ()

indent :: Parser ()
indent = many1 nonBreakSpace >> pure ()

exitCode :: Parser Int
exitCode = tok decimal

instr :: (Text, Parser AsmInstruction) -> Parser AsmInstruction
instr (op, p) = (lit op) *> p

instruction :: Declarations -> Parser AsmInstruction
instruction ds = choice . Protolude.map instr $ [
    ("remove-all", pure RemoveAll),
    ("list", List <$> key),
    ("create", Create <$> deviceRef ds),
    ("remove", Remove <$> deviceRef ds),
    ("suspend", Suspend <$> deviceRef ds),
    ("resume", Resume <$> deviceRef ds),
    ("load", Load <$> deviceRef ds <*> tableRef ds),
    ("info", InfoQ <$> key <*> deviceRef ds),
    ("table", TableQ <$> key <*> deviceRef ds),
    ("begin", pure BeginObject),
    ("end", EndObject <$> key),
    ("literal", Literal <$> key <*> value),
    ("jmp", Jmp <$> labelRef),
    ("jmp-fail", JmpFail <$> labelRef),
    ("exit", Exit <$> exitCode)]

endLine :: Parser ()
endLine = (many nonBreakSpace) *> (endOfInput <|> endOfLine)

-- Make sure p doesn't accept whitespace at the start
bol :: Parser a -> Parser a
bol p = p <* endLine

indented :: Parser a -> Parser a
indented p = indent *> p

blankLine :: Parser ()
blankLine = do
    _ <- many nonBreakSpace
    endOfLine
    pure ()

labelOrInstr :: Declarations -> Parser AsmInstruction
labelOrInstr ds = do
    _ <- many (Data.Attoparsec.Text.try blankLine)
    labelInstr <|> (indented $ instruction ds)

instructions :: Declarations -> Parser [AsmInstruction]
instructions ds = sepBy (labelOrInstr ds) endLine

stripLabels :: [AsmInstruction] -> Parser [I.Instruction]
stripLabels code = join <$> mapM toInstr code
    where
        toInstr :: AsmInstruction -> Parser [I.Instruction]
        toInstr (Label _) = pure []
        toInstr (RemoveAll) = pure [I.RemoveAll]
        toInstr (List n) = pure [I.List n]
        toInstr (Create dev) = pure [I.Create dev]
        toInstr (Remove dev) = pure [I.Remove dev]
        toInstr (Suspend dev) = pure [I.Suspend dev]
        toInstr (Resume dev) = pure [I.Resume dev]
        toInstr (Load dev t) = pure [I.Load dev t]
        toInstr (InfoQ n dev) = pure [I.InfoQ n dev]
        toInstr (TableQ n dev) = pure [I.TableQ n dev]
        toInstr BeginObject = pure [I.BeginObject]
        toInstr (EndObject n) = pure [I.EndObject n]
        toInstr (Literal k v) = pure [I.Literal k v]
        toInstr (Jmp name) = do
            dest <- lookupLabel name
            pure [I.Jmp dest]
        toInstr (JmpFail name) = do
            dest <- lookupLabel name
            pure [I.JmpFail dest]
        toInstr (Exit c) = pure [I.Exit c]

        lookupLabel :: Text -> Parser Int
        lookupLabel name = case M.lookup name labelTable of
            Just pc -> pure pc
            Nothing -> fail $ "unable to find label '" ++ (T.unpack name) ++ "'"

        toTable :: [AsmInstruction] -> Int -> M.Map Text Int
        toTable [] _ = M.empty
        toTable ((Label name):xs) pc = M.insert name pc (toTable xs pc)
        toTable (_:xs) pc = toTable xs (pc + 1)

        labelTable = toTable code 0

assemble :: [AsmInstruction] -> Parser I.Program
assemble code = I.mkProgram <$> stripLabels code

program :: Parser I.Program
program = decls >>= instructions >>= assemble

buildError :: [Text] -> Text -> Text
buildError _ msg = msg

parseAsm :: Text -> Either Text I.Program
parseAsm input = case parse program input of
    Fail _ ctxts msg -> Left $ buildError (fmap T.pack ctxts) (T.pack msg)
    Partial _ -> Left $ "incomplete input"
    Done _ r -> Right r

----------------------------------------------------
