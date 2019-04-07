{-# LANGUAGE OverloadedStrings #-}

module Data.Jack.Token.ConversionTo.ByteString.XML where

import qualified Data.Jack.Token.Model as TK
import qualified Data.Output.Model as OUT

import qualified Data.ByteString.Char8 as BS
import System.FilePath (addExtension, dropExtension)

convertDirectory :: [TK.File] -> [OUT.OutputFile]
convertDirectory = map convertSingleFile

convertSingleFile :: TK.File -> OUT.OutputFile
convertSingleFile TK.File { TK.program = tkProg
                , TK.path    = tkPath } =
    OUT.OutputFile { OUT.outputProgram = convertTokens tkProg
                   , OUT.path          = addExtension (dropExtension tkPath <> "T") ".xml" }

class ToXML a where
    toXML :: a -> BS.ByteString

convertTokens :: [TK.Token] -> BS.ByteString
convertTokens tks = 
        "<tokens>\n"
    <>  foldr ((<>) . toXML) "" tks 
    <> "</tokens>\n"

instance ToXML TK.Keyword where
    toXML t =
        case t of
            TK.Class -> "class"
            TK.Constructor -> "constructor"
            TK.Function -> "function"
            TK.Method -> "method"
            TK.Field -> "field"
            TK.Static -> "static"
            TK.Var -> "var"
            TK.Int -> "int"
            TK.Char -> "char"
            TK.Boolean -> "boolean"
            TK.Void -> "void"
            TK.TrueKW -> "true"
            TK.FalseKW -> "false"
            TK.Null -> "null"
            TK.This -> "this"
            TK.Let -> "let"
            TK.Do -> "do"
            TK.If -> "if"
            TK.Else -> "else"
            TK.While -> "while"
            TK.Return -> "return"

instance ToXML TK.Symbol where
    toXML sym =
        case sym of
            TK.LCurlyBracket -> "{"
            TK.RCurlyBracket -> "}"
            TK.LParen -> "("
            TK.RParen -> ")"
            TK.LSquareBracket -> "["
            TK.RSquareBracket -> "]"
            TK.FullStop -> "."
            TK.Comma -> ","
            TK.Semicolon -> ";"
            TK.Plus -> "+"
            TK.Minus -> "-"
            TK.Asterix -> "*"
            TK.Slash -> "/"
            TK.Ampersand -> "&amp;"
            TK.VerticalBar -> "|"
            TK.LAngleBracket -> "&lt;"
            TK.RAngleBracket -> "&gt;"
            TK.Equal -> "="
            TK.Tilde -> "~"

instance ToXML TK.Token where
    toXML (TK.KW kw) = 
            "<keyword> "
        <>  toXML kw
        <>  " </keyword>\n"
    toXML (TK.SY sy) = 
            "<symbol> "
        <>  toXML sy
        <>  " </symbol>\n"
    toXML (TK.IC ic) = 
            "<integerConstant> "
        <>  (BS.pack . show) ic
        <>  " </integerConstant>\n"
    toXML (TK.SC sc) =
            "<stringConstant> "
        <>  sc
        <>  " </stringConstant>\n"
    toXML (TK.ID iden) =
            "<identifier> "
        <>  iden
        <>  " </identifier>\n"

