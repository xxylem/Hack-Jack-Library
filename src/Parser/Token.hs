{-# LANGUAGE OverloadedStrings #-}

module Parser.Token where

import qualified Data.Jack.Token.Model as TK
import qualified Data.Source.Model as S

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS


import Prelude hiding (takeWhile)

tokeniseFile :: S.UnparsedBSFile -> Either String TK.File
tokeniseFile S.UnparsedBSFile { S.unparsedByteString = bsProg
                              , S.bsPath               = srcPath } = do
        tkns <- parseOnly parseTokens bsProg
        return $ TK.File { TK.program = tkns
                         , TK.path    = srcPath }

tokeniseFiles :: [S.UnparsedBSFile] -> Either String [TK.File]
tokeniseFiles [] = return []
tokeniseFiles (f:fs) = do
    f' <- tokeniseFile f
    fs' <- tokeniseFiles fs
    return $ f':fs'

parseTokens :: Parser [TK.Token]
parseTokens = manyTill parseToken endOfInput

parseToken :: Parser TK.Token
parseToken =
        skipSpace
    *>  skipComments
    *>
    (       (TK.KW <$> parseKeyword)
        <|> (TK.SY <$> parseSymbol)
        <|> (TK.IC <$> parseIntegerConstant)
        <|> (TK.SC <$> parseStringConstant)
        <|> (TK.ID <$> parseIdentifier)
    )
    <*  skipSpace
    <*  skipComments

identifierCharacters :: [Char]
identifierCharacters =
        '_'
    :   ['a'..'z']
    <>  ['A'..'Z']
    <>  ['0'..'9']

take1Space :: Parser ()
take1Space =    
        (
            (   
                char ' '
            <|> char '\t'
            )
        >> return ()
        )
    <|> endOfLine
    <|> endOfInput
    <|> (do
        c <- peekChar'
        if c `notElem` identifierCharacters
            then return ()
            else fail "expected non-identifier character")

parseKeyword :: Parser TK.Keyword
parseKeyword =
    (
        (string "class"         >> return TK.Class)
    <|> (string "constructor"   >> return TK.Constructor)
    <|> (string "function"      >> return TK.Function)
    <|> (string "method"        >> return TK.Method)
    <|> (string "field"         >> return TK.Field)
    <|> (string "static"        >> return TK.Static)
    <|> (string "var"           >> return TK.Var)
    <|> (string "int"           >> return TK.Int)
    <|> (string "char"          >> return TK.Char)
    <|> (string "boolean"       >> return TK.Boolean)
    <|> (string "void"          >> return TK.Void)
    <|> (string "true"          >> return TK.TrueKW)
    <|> (string "false"         >> return TK.FalseKW)
    <|> (string "null"          >> return TK.Null)
    <|> (string "this"          >> return TK.This)
    <|> (string "let"           >> return TK.Let)
    <|> (string "do"            >> return TK.Do)
    <|> (string "if"            >> return TK.If)
    <|> (string "else"          >> return TK.Else)
    <|> (string "while"         >> return TK.While)
    <|> (string "return"        >> return TK.Return)
    )
    <* take1Space


parseSymbol :: Parser TK.Symbol
parseSymbol =
        (char '{' >> return TK.LCurlyBracket)
    <|> (char '}' >> return TK.RCurlyBracket)
    <|> (char '(' >> return TK.LParen)
    <|> (char ')' >> return TK.RParen)
    <|> (char '[' >> return TK.LSquareBracket)
    <|> (char ']' >> return TK.RSquareBracket)
    <|> (char '.' >> return TK.FullStop)
    <|> (char ',' >> return TK.Comma)
    <|> (char ';' >> return TK.Semicolon)
    <|> (char '+' >> return TK.Plus)
    <|> (char '-' >> return TK.Minus)
    <|> (char '*' >> return TK.Asterix)
    <|> (char '/' >> return TK.Slash)
    <|> (char '&' >> return TK.Ampersand)
    <|> (char '|' >> return TK.VerticalBar)
    <|> (char '<' >> return TK.LAngleBracket)
    <|> (char '>' >> return TK.RAngleBracket)
    <|> (char '=' >> return TK.Equal)
    <|> (char '~' >> return TK.Tilde)

parseIntegerConstant :: Parser TK.IntegerConstant
parseIntegerConstant = decimal

parseStringConstant :: Parser TK.StringConstant
parseStringConstant =
        char '\"'
    >>  takeWhile1 (notInClass "\"\n")
    <*  char '\"'

parseIdentifier :: Parser TK.Identifier
parseIdentifier = do
    firstLetter <-  satisfy (inClass "a-zA-Z_")
    rest        <-  takeWhile (inClass "0-9a-zA-Z_")
    return (BS.cons firstLetter rest)


skipLineComment :: Parser ()
skipLineComment =
        string "//"
    *>  manyTill anyChar (char '\n')
    *>  return ()

skipAPIComment :: Parser ()
skipAPIComment =
        string "/**"
    *>  manyTill anyChar (string "*/")
    *>  return ()

skipBlockComment :: Parser ()
skipBlockComment =
        string "/*"
    *>  manyTill anyChar (string "*/")
    *>  return ()

skipComment :: Parser ()
skipComment =
        skipLineComment
    <|> skipAPIComment
    <|> skipBlockComment

skipComments :: Parser ()
skipComments =
    skipMany (skipSpace >> skipComment >> skipSpace)