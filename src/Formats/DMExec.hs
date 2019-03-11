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

type Declarations = M.Map ByteString Decl

around :: Parser a -> Parser b -> Parser c -> Parser c
around open close p = open *> p <* close

tok :: Parser a -> Parser a
tok p = skipSpace *> p <* skipSpace

lit :: ByteString -> Parser ByteString
lit = tok . string

identifier :: Parser ByteString
identifier = tok (BS.pack <$> many' (satisfy idChar))
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

quotedString :: Parser ByteString
quotedString = tok (doubleQuote *> (BS.pack <$> (many' escapedChar)) <* doubleQuote)

reqPair :: ByteString -> Parser ByteString
reqPair key = tok $ do
    k <- identifier
    guard (k == key)
    lit "="
    quotedString

deviceDecl :: Parser (ByteString, Decl)
deviceDecl = do
    lit "device"
    var <- identifier
    lit "="
    lit "{"
    name <- reqPair "name"
    uuid <- (Just <$> (lit "," *> reqPair "uuid")) <|> return Nothing
    lit "}"
    return (var, Dev (DeviceId (E.decodeUtf8 name) (E.decodeUtf8 <$> uuid)))

sectors :: Parser Integer
sectors = read <$> many digit

targetType :: Parser Text
targetType = E.decodeUtf8 <$> identifier

tableLine :: Parser TableLine
tableLine = TableLine <$> targetType <*> sectors <*> (E.decodeUtf8 <$> quotedString)

table :: Parser Decl
table = Table <$> around (lit "[") (lit "]") (many' tableLine)

tableDecl :: Parser (ByteString, Decl)
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
    I.Label <$> (E.decodeUtf8 <$> identifier)

key :: Parser Text
key = undefined

value :: Parser Text
value = undefined

deviceRef :: Declarations -> Parser DeviceId
deviceRef decls = undefined

tableRef :: Declarations -> Parser [TableLine]
tableRef decls = undefined

labelRef :: Parser Text
labelRef = undefined

indent :: Parser ()
indent = undefined

exitCode :: Parser Int
exitCode = undefined

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
