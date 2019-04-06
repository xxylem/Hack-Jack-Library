{-# LANGUAGE OverloadedStrings #-}

module Parser.ASM where

import qualified Data.Hack.ASM.Model as ASM
import qualified Data.Source.Model as S
import qualified Parser.Error as PE

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 (Parser, skipSpace, char, decimal, manyTill',
                                        anyChar, eitherP, endOfInput, string, parseOnly)
import Data.Attoparsec.Combinator (lookAhead)
import qualified Data.ByteString.Char8 as BS (pack)

parseASMFile :: S.UnparsedFile -> Either PE.ParseError ASM.File
parseASMFile S.UnparsedFile { S.unparsedProgram = srcProg
                            , S.path            = srcPath } = do
        instructions <- runParseLines srcProg
        return $ ASM.makeFileFromInstructions srcPath instructions

-- ====================================================================================== --

runParseLines :: S.UnparsedProgram -> Either PE.ParseError [ASM.Instruction]
runParseLines [] = Right []
runParseLines (l:ls) =
    case parseOnly parseInstruction (S.code l) of
        Right i -> (:) <$> Right i <*> runParseLines ls
        Left  err -> case parseOnly parseComment (S.code l) of
                        Right _ -> runParseLines ls
                        Left err' -> Left $ PE.ParseError { PE.message = err <> err' 
                                                          , PE.line    = l }


-- Checks for A instructions, C instructions and labels for one line of code
-- Also checks that the rest of the line is valid (i.e. contains only empty space, a comment,
-- or is the end of the line).
parseInstruction :: Parser ASM.Instruction
parseInstruction = 
        skipSpace 
    >>  (   parseComputationInstruction
        <|> parseAddressInstruction
        <|> parseAddressSymbol
        <|> parseAddressLabel
        )
    <*  parseComment

-- ====================================================================================== --
-- A Instructions --

-- Basic address instructions with only a number, 
-- e.g. @17, @25093
parseAddressInstruction :: Parser ASM.Instruction
parseAddressInstruction =
        skipSpace
    >>  char '@' 
    >>  ASM.A <$> decimal 

-- Returns the symbol string from an A instruction containing a symbol/label ref
-- e.g. @LOOP, @counter
parseAddressSymbol :: Parser ASM.Instruction
parseAddressSymbol = do
    skipSpace
    _ <- char '@'
    sym <- manyTill' anyChar   (lookAhead  (eitherP    endOfInput
                                                    (       char ' '
                                                        <|> char '/'
                                                        <|> char '\r' )))
    parseComment
    return $ ASM.S $ BS.pack sym

-- Returns the label (as a string) from a label declaration
-- e.g. (LOOP), (END)
parseAddressLabel :: Parser ASM.Instruction
parseAddressLabel = do
    skipSpace
    _   <- char '('
    lb  <- manyTill' anyChar (char ')') 
    parseComment
    return $ ASM.L $ BS.pack lb

-- ====================================================================================== --
-- C Instructions --

-- Combines the parsers into one that deals with the full C instruction
-- e.g. parses all parts of "M=A+1;JMP" into their relevant components and combines them
-- to a full Instruction.
parseComputationInstruction :: Parser ASM.Instruction
parseComputationInstruction = do
    skipSpace
    dest <- parseDestination
    skipSpace
    comp <- parseComputation
    skipSpace
    jump <- parseJump
    return $ ASM.C { ASM.computation=comp, ASM.destination=dest, ASM.jump=jump }

-- Parses the <<computation>> section of the C instruction.
-- e.g. the "A+1" in the instruction "M=A+1;JMP"
-- ASSUMES: The optional destination section of the C instruction has already been parsed.
parseComputation :: Parser ASM.Computation
parseComputation =
        (char '0'       >> return ASM.ZERO)
    <|> (char '1'       >> return ASM.ONE)
    <|> (string "-1"    >> return ASM.MINUS_ONE)
    <|> (string "!D"    >> return ASM.NOT_D)
    <|> (string "!A"    >> return ASM.NOT_A)
    <|> (string "-D"    >> return ASM.NEG_D)
    <|> (string "-A"    >> return ASM.NEG_A)
    <|> (string "D+1"   >> return ASM.D_PLUS_1)
    <|> (string "A+1"   >> return ASM.A_PLUS_1)
    <|> (string "D-1"   >> return ASM.D_MINUS_1)
    <|> (string "A-1"   >> return ASM.A_MINUS_1)
    <|> (string "D+A"   >> return ASM.D_PLUS_A)
    <|> (string "D-A"   >> return ASM.D_MINUS_A)
    <|> (string "A-D"   >> return ASM.A_MINUS_D)
    <|> (string "D&A"   >> return ASM.D_AND_A)
    <|> (string "D|A"   >> return ASM.D_OR_A)
    <|> (string "!M"    >> return ASM.NOT_M)
    <|> (string "-M"    >> return ASM.NEG_M)
    <|> (string "M+1"   >> return ASM.M_PLUS_1)
    <|> (string "M-1"   >> return ASM.M_MINUS_1)
    <|> (string "D+M"   >> return ASM.D_PLUS_M)
    <|> (string "D-M"   >> return ASM.D_MINUS_M)
    <|> (string "M-D"   >> return ASM.M_MINUS_D)
    <|> (string "D&M"   >> return ASM.D_AND_M)
    <|> (string "D|M"   >> return ASM.D_OR_M)
    <|> (char 'D'       >> return ASM.D_COMP)
    <|> (char 'A'       >> return ASM.A_COMP)
    <|> (char 'M'       >> return ASM.M_COMP)

-- Parses the <<destination>> section of the C instruction
-- If the <<destination>> section is not found, returns NULL_JUMP, assuming it
-- was intentionally omited.
-- e.g. the "M=" in the instruction "M=A+1;JMP"
parseDestination :: Parser ASM.Destination
parseDestination =
        (   (   (string "AMD" >> return ASM.AMD)
            <|> (string "AM" >> return ASM.AM)
            <|> (string "AD" >> return ASM.AD)
            <|> (string "MD" >> return ASM.MD)
            <|> (char 'M' >> return ASM.M_DEST)
            <|> (char 'A' >> return ASM.A_DEST)
            <|> (char 'D' >> return ASM.D_DEST)
            ) 
            <*  char '=')
    <|> return ASM.NULL_DEST

-- Parses the <<jump>> section of the C instruction
-- e.g. the ";JMP" in the instruction "M=A+1;JMP"
-- If the <<jump>> section is not found, returns NULL_JUMP, assuming it
-- was intentionally omited.
-- ASSUMES: all previous parts of the C instruction have been parsed and consumed.
parseJump :: Parser ASM.Jump
parseJump =
        (   char ';' 
        >>
            (   (string "JGT" >> return ASM.JGT)
            <|> (string "JEQ" >> return ASM.JEQ)
            <|> (string "JGE" >> return ASM.JGE)
            <|> (string "JLT" >> return ASM.JLT)
            <|> (string "JNE" >> return ASM.JNE)
            <|> (string "JLE" >> return ASM.JLE)
            <|> (string "JMP" >> return ASM.JMP)
            )
        )
    <|> return ASM.NULL_JUMP

-- ====================================================================================== --

-- Checks for comments, whitespace, or end of line, e.g. "   //this is a comment"
parseComment :: Parser ()
parseComment =
        skipSpace 
    >>  (   (string "//" >> return ())
        <|> endOfInput
        )
