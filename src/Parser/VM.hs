{-# lANGUAGE OverloadedStrings #-}

module Parser.VM where

import qualified Data.VM.Model as VM
import qualified Data.Source.Model as S
import qualified Parser.Error as PE


import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8

parseVMFiles :: [S.UnparsedFile] -> Either PE.ParseError [VM.File]
parseVMFiles [] = return []
parseVMFiles (f:fs) = (:) <$> (parseVMFile f) <*> parseVMFiles fs

parseVMFile :: S.UnparsedFile -> Either PE.ParseError VM.File
parseVMFile S.UnparsedFile { S.unparsedProgram = srcProg
                           , S.path            = srcPath } =
    case parseVMLines 0 srcProg of
        Right vmProg -> return $ VM.File { VM.program = vmProg
                                         , VM.path    = srcPath }
        Left  err    -> Left err

parseVMLines :: Integer -> S.UnparsedProgram -> Either PE.ParseError VM.Program
parseVMLines _              []      = return []
parseVMLines nextLineNumber (l:ls) =
    case parseOnly parseVMLine (S.code l) of
        Right i -> (:) <$> Right (VM.Line { VM.lineNumber = nextLineNumber
                                           , VM.instruction = i})
                        <*> parseVMLines (nextLineNumber + 1) ls
        Left err -> case parseOnly parseComment (S.code l) of
                        Right _ -> parseVMLines nextLineNumber ls
                        Left err' -> Left $ PE.ParseError { PE.line = l
                                                          , PE.message = err <> err' }

-- ====== --
-- Parsers --

parseDirection :: Parser VM.Direction
parseDirection =
        (string "push" >> return VM.PUSH)
    <|> (string "pop"  >> return VM.POP)

parseSegment :: Parser VM.Segment
parseSegment =
        (string "argument" >> return VM.ARGUMENT)
    <|> (string "local"    >> return VM.LOCAL)
    <|> (string "static"   >> return VM.STATIC)
    <|> (string "constant" >> return VM.CONSTANT)
    <|> (string "this"     >> return VM.THIS)
    <|> (string "that"     >> return VM.THAT)
    <|> (string "pointer"  >> return VM.POINTER)
    <|> (string "temp"     >> return VM.TEMP)

parseMemoryAccessCMD :: Parser VM.MemoryAccessCommand
parseMemoryAccessCMD = do
    direction <- parseDirection
    skipMany1 (char ' ')
    segment   <- parseSegment
    skipMany1 (char ' ')
    index     <- decimal
    parseComment
    return $  VM.MemCMD direction segment index

parseArithLogicCMD :: Parser VM.ArithLogicCommand
parseArithLogicCMD =
    (       (string "add" >> return VM.ADD)
        <|> (string "sub" >> return VM.SUB)
        <|> (string "neg" >> return VM.NEG)
        <|> (string "eq"  >> return VM.EQ_VM)
        <|> (string "gt"  >> return VM.GT_VM)
        <|> (string "lt"  >> return VM.LT_VM)
        <|> (string "and" >> return VM.AND)
        <|> (string "or"  >> return VM.OR)
        <|> (string "not" >> return VM.NOT)
    )
    <*  parseComment

parseLabelCommand :: Parser VM.ProgramFlowCommand
parseLabelCommand = 
        string "label"
    >>  skipMany1 (char ' ')
    >>  VM.LABEL <$> takeWhile1 (not . isSpace)

parseGotoCommand :: Parser VM.ProgramFlowCommand
parseGotoCommand =
        string "goto"
    >>  skipMany1 (char ' ')
    >>  VM.GOTO <$> takeWhile1 (not . isSpace)

parseIfGotoCommand :: Parser VM.ProgramFlowCommand
parseIfGotoCommand =
        string "if-goto"
    >>  skipMany1 (char ' ')
    >>  VM.IF_GOTO <$> takeWhile1 (not . isSpace)

parseProgramFlowCommand :: Parser VM.ProgramFlowCommand
parseProgramFlowCommand =
    (       parseLabelCommand
        <|> parseGotoCommand
        <|> parseIfGotoCommand
    )
    <*  parseComment

parseFnCommand :: Parser VM.FunctionCommand
parseFnCommand = do
    _       <- string "function"
    skipMany1 (char ' ')
    fnName  <- takeWhile1 (not . isSpace)
    skipMany1 (char ' ')
    nVars   <- decimal
    return $ VM.FUN fnName nVars

parseCallCommand :: Parser VM.FunctionCommand
parseCallCommand = do
    _       <- string "call"
    skipMany1 (char ' ')
    fnName  <- takeWhile1 (not . isSpace)
    skipMany1 (char ' ')
    nArgs   <- decimal
    return $ VM.CALL fnName nArgs

parseReturnCommand :: Parser VM.FunctionCommand
parseReturnCommand =
        string "return"
    >>  return VM.RETURN

parseFunctionCommand :: Parser VM.FunctionCommand
parseFunctionCommand =
    (       parseFnCommand
        <|> parseCallCommand
        <|> parseReturnCommand
    )
    <*  parseComment

parseVMLine :: Parser VM.Instruction
parseVMLine = 
        (VM.AL_VM <$> parseArithLogicCMD)
    <|> (VM.M_VM  <$> parseMemoryAccessCMD)
    <|> (VM.P_VM  <$> parseProgramFlowCommand)
    <|> (VM.F_VM  <$> parseFunctionCommand)

parseComment :: Parser ()
parseComment =
        skipSpace 
    >>  (   (string "//" >> return ())
        <|> endOfInput
        )