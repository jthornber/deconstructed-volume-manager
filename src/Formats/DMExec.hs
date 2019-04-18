{-# LANGUAGE OverloadedStrings #-}

module Formats.DMExec (
    Decl (..),
    parseAsm,

    -- only exported for testing
    Declarations (..),
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
    indented,
    foo
    ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.ByteString
import Data.Char
import Data.Map.Strict as M
import Data.Text (Text)
import Data.Text.Encoding as E
import Data.Text as T
import DeviceMapper.Instructions as I
import DeviceMapper.LowLevelTypes

-- Declarations are used to predefine literals to make the instructions
-- more succinct

data Decl =
    Dev DeviceId |
    Table [TableLine]
    deriving (Eq, Show)

type Declarations = M.Map Text Decl

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

identifier :: Parser Text
identifier = tok $ do
    c <- satisfy isAlpha
    cs <- Data.Attoparsec.Text.takeWhile idChar
    pure $ T.cons c cs
    where
        idChar c = isDigit c || isAlpha c

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
reqPair key = do
    k <- identifier
    guard (k == key)
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

decls :: Parser Declarations
decls = M.fromList <$> many' (deviceDecl <|> tableDecl)

label :: Parser I.Instruction
label = char '.' *> (I.Label <$> identifier)

key :: Parser Text
key = quotedString

value :: Parser Text
value = quotedString

deviceRef :: Declarations -> Parser DeviceId
deviceRef decls = do
    var <- identifier
    case M.lookup var decls of
       (Just (Dev d)) -> pure d
       (Just _) -> fail "expected DeviceId, but got Table"
       _ -> fail "no such DeviceId"

tableRef :: Declarations -> Parser [TableLine]
tableRef decls = do
    var <- identifier
    case M.lookup var decls of
       (Just (Table t)) -> pure t
       (Just _) -> fail "expected Table, but got DeviceId"
       _ -> fail "no such Table"

labelRef :: Parser Text
labelRef = identifier

indent :: Parser ()
indent = many1 (char ' ' <|> char '\t') >> pure ()

exitCode :: Parser Int
exitCode = tok decimal

instr (op, p) = (lit op) *> p

instruction :: Declarations -> Parser I.Instruction
instruction decls = choice . Prelude.map instr $ [
    ("remove-all", pure I.RemoveAll),
    ("list", I.List <$> key),
    ("create", I.Create <$> deviceRef decls),
    ("remove", I.Remove <$> deviceRef decls),
    ("suspend", I.Suspend <$> deviceRef decls),
    ("resume", I.Resume <$> deviceRef decls),
    ("load", I.Load <$> deviceRef decls <*> tableRef decls),
    ("info", I.InfoQ <$> key <*> deviceRef decls),
    ("table", I.TableQ <$> key <*> deviceRef decls),
    ("begin", pure I.BeginObject),
    ("end", I.EndObject <$> key),
    ("literal", I.Literal <$> key <*> value),
    ("jmp", I.Jmp <$> labelRef),
    ("jmp-fail", I.JmpFail <$> labelRef),
    ("exit", I.Exit <$> exitCode)]

endLine = space' *> (endOfInput <|> endOfLine)

-- Make sure p doesn't accept whitespace at the start
bol :: Parser a -> Parser a
bol p = p <* endLine

indented :: Parser a -> Parser a
indented p = indent *> p <* endLine

instructions :: Declarations -> Parser I.Program
instructions decls = I.mkProgram <$> many' ((bol label) <|> (indented $ instruction decls))

prog :: Parser I.Program
prog = decls >>= instructions

buildError :: [String] -> String -> Text
buildError _ msg = T.pack msg

parseAsm :: Text -> Either Text I.Program
parseAsm input = case parse prog input of
    Fail _ ctxts msg -> Left $ buildError ctxts msg
    Partial _ -> Left $ "incomplete input"
    Done _ r -> Right r
