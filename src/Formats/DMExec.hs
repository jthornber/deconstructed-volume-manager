{-# LANGUAGE OverloadedStrings #-}

module Formats.DMExec (
    parseAsm
        ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 as BS
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

-- FIXME: handle comments from within tok
tok :: Parser a -> Parser a
tok p = skipSpace *> p <* skipSpace

lit :: ByteString -> Parser ByteString
lit = tok . string

identifier :: Parser Text
identifier = E.decodeUtf8 <$> tok (BS.pack <$> many' (satisfy idChar))
    where
        idChar c = isDigit c || isAlpha_ascii c

doubleQuote :: Parser ()
doubleQuote = char '"' *> return ()

escapedChar :: Parser Char
escapedChar = do
    c <- notChar '"'
    case c of
        '\\' -> anyChar
        _ -> return c

quotedString :: Parser Text
quotedString = E.decodeUtf8 <$> tok (doubleQuote *> (BS.pack <$> (many' escapedChar)) <* doubleQuote)

reqPair :: Text -> Parser Text
reqPair key = tok $ do
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
    uuid <- (Just <$> (lit "," *> reqPair "uuid")) <|> return Nothing
    lit "}"
    return (var, Dev (DeviceId name uuid))

sectors :: Parser Integer
sectors = read <$> many digit

targetType :: Parser Text
targetType = identifier

tableLine :: Parser TableLine
tableLine = TableLine <$> targetType <*> sectors <*> quotedString

table :: Parser Decl
table = Table <$> around (lit "[") (lit "]") (many' tableLine)

tableDecl :: Parser (Text, Decl)
tableDecl = do
    lit "table"
    var <- identifier
    lit "="
    d <- table
    return (var, d)

decls :: Parser Declarations
decls = M.fromList <$> many' (deviceDecl <|> tableDecl)

label :: Parser I.Instruction
label = do
    char '.'
    I.Label <$> identifier

key :: Parser Text
key = quotedString

value :: Parser Text
value = quotedString

deviceRef :: Declarations -> Parser DeviceId
deviceRef decls = do
    var <- identifier
    case M.lookup var decls of
       (Just (Dev d)) -> return d
       (Just _) -> fail "expected DeviceId, but got Table"
       _ -> fail "no such DeviceId"

tableRef :: Declarations -> Parser [TableLine]
tableRef decls = do
    var <- identifier
    case M.lookup var decls of
       (Just (Table t)) -> return t
       (Just _) -> fail "expected Table, but got DeviceId"
       _ -> fail "no such Table"

labelRef :: Parser Text
labelRef = identifier

indent :: Parser ()
indent = many1 (char ' ' <|> char '\t') >> return ()

exitCode :: Parser Int
exitCode = do
    n <- read <$> many' digit
    guard (n < 256)
    return n

instruction :: Declarations -> Parser I.Instruction
instruction decls = do
    indent
    op <- identifier
    case op of
        "remove-all" -> return I.RemoveAll
        "list" -> I.List <$> key
        "create" -> I.Create <$> deviceRef decls
        "remove" -> I.Remove <$> deviceRef decls
        "suspend" -> I.Suspend <$> deviceRef decls
        "resume" -> I.Resume <$> deviceRef decls
        "load" -> I.Load <$> deviceRef decls <*> tableRef decls
        "info" -> I.InfoQ <$> key <*> deviceRef decls
        "table" -> I.TableQ <$> key <*> deviceRef decls
        "begin" -> return I.BeginObject
        "end" -> I.EndObject <$> (E.decodeUtf8 <$> "")
        "literal" -> I.Literal <$> key <*> value
        "jmp" -> I.Jmp <$> labelRef
        "jmp-fail" -> I.JmpFail <$> labelRef
        "exit" -> I.Exit <$> exitCode
        _ -> fail "bad instruction"

-- FIXME: look for the .start label
instructions :: Declarations -> Parser I.Program
instructions decls = I.mkProgram 0 <$> many' (label <|> instruction decls)

prog :: Parser I.Program
prog = decls >>= instructions

buildError :: [String] -> String -> Text
buildError _ msg = T.pack msg

parseAsm :: ByteString -> Either Text I.Program
parseAsm input = case parse prog input of
    Fail _ ctxts msg -> Left $ buildError ctxts msg
    Partial _ -> Left $ "incomplete input"
    Done _ r -> Right r
