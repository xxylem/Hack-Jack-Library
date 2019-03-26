{-# LANGUAGE OverloadedStrings #-}

module Parser.ASM where

import qualified Data.Hack.ASM.Model as ASM
import qualified Data.Source.Model as S

import Control.Applicative ((<|>))
import Control.Monad.Trans.State
import Data.Attoparsec.ByteString.Char8 (Parser, skipSpace, char, decimal, manyTill',
                                        anyChar, eitherP, endOfInput, string, parseOnly)
import Data.Attoparsec.Combinator (lookAhead)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map

parseASMFile :: BS.ByteString -> Either ParseError ASM.Program
parseASMFile bs = do
    let noCommsFile = removeCommentsAndEmptyLines $ S.toUnparsedFile 1 bs
        (noSymsFile, st) = runState (moveLabelsToSymbolTable noCommsFile) initState
    evalState (runParseInstructionLines noSymsFile) st

removeCommentsAndEmptyLines :: S.UnparsedFile -> S.UnparsedFile
removeCommentsAndEmptyLines = filter (not . runParseIsEmptyLineOrComment)
            where runParseIsEmptyLineOrComment l =
                    case parseOnly parseComment (S.code l) of
                        (Right _) -> True
                        (Left _)  -> False

data ErrorType =
        InvalidLine
    |   NoSymbol

data ParseError = 
    ParseError { errorType :: ErrorType
            , message   :: String
            , line      :: S.UnparsedLine}

data ParseState = ParseState { nextAddressValue :: Integer 
                             , nextLineNumber   :: Integer
                             , symbolTable      :: SymbolTable
                               }

incNextAddValue :: State ParseState ()
incNextAddValue = do
    st <- get
    put $ st { nextAddressValue = 1 + nextAddressValue st }
incNextLineNumber :: State ParseState ()
incNextLineNumber = do
    st <- get
    put $ st { nextLineNumber = 1 + nextLineNumber st }
insertToSymbolTable :: Label -> AddressVal -> State ParseState ()
insertToSymbolTable label aVal = do
    st <- get
    put $ st { symbolTable = Map.insert label aVal (symbolTable st) }


type Label = String
type AddressVal = Integer
type SymbolTable = Map.Map Label AddressVal

initState :: ParseState
initState = ParseState { nextAddressValue=16
                       , nextLineNumber=0
                       , symbolTable=initSymbolTable
                       }

initSymbolTable :: SymbolTable
initSymbolTable =
    Map.fromList [  ("SP",      0)
                 ,  ("LCL",     1)
                 ,  ("ARG",     2)
                 ,  ("THIS",    3)
                 ,  ("THAT",    4)
                 ,  ("R0",      0)
                 ,  ("R1",      1)
                 ,  ("R2",      2)
                 ,  ("R3",      3)
                 ,  ("R4",      4)
                 ,  ("R5",      5)
                 ,  ("R6",      6)
                 ,  ("R7",      7)
                 ,  ("R8",      8)
                 ,  ("R9",      9)
                 ,  ("R10",     10)
                 ,  ("R11",     11)
                 ,  ("R12",     12)
                 ,  ("R13",     13)
                 ,  ("R14",     14)
                 ,  ("R15",     15)
                 ,  ("SCREEN",  16384)
                 ,  ("KBD",     24576)
                 ]

-- ====================================================================================== --

-- Parses an instruction on one line, including @symbol instructions. 
runParseInstructionLine :: S.UnparsedLine -> State ParseState (Either ParseError ASM.Line)
runParseInstructionLine l = do
    st <- get
    case parseOnly parseInstruction (S.code l) of
        (Right instr) -> do
                     let lNumber = nextLineNumber st
                     incNextLineNumber
                     return $ Right (ASM.Line { ASM.lineNumber = lNumber
                                              , ASM.instruction = instr })
        (Left _) -> runParseSymbol l

-- Takes an unparsed line and attempts to parse a @symbol instruction, where symbol
-- is not an integer. If the symbol is found in the symbol table, returns an instruction
-- with the resolvd address location. If the symbol isn't found, adds the symbol
-- to the symbol table at the next available address.
runParseSymbol :: S.UnparsedLine -> State ParseState (Either ParseError ASM.Line)
runParseSymbol l = do
    st <- get
    let symTab = symbolTable st
    case parseOnly parseAddressSymbol (S.code l) of
        (Right sym) -> case Map.lookup sym symTab of
                            Just val -> do
                                        let lNumber = nextLineNumber st
                                        incNextLineNumber
                                        return $ Right 
                                                (ASM.Line { ASM.lineNumber = lNumber
                                                          , ASM.instruction = ASM.A val })
                            Nothing  -> do
                                        let aVal    = nextAddressValue st
                                            lNumber = nextLineNumber st
                                        insertToSymbolTable sym aVal
                                        incNextAddValue
                                        incNextLineNumber
                                        return $ Right 
                                                (ASM.Line { ASM.lineNumber = lNumber
                                                          , ASM.instruction = ASM.A aVal} )
        (Left err)    -> return $ Left (ParseError { errorType=NoSymbol
                                           , message=err
                                           , line=l })


runParseInstructionLines :: S.UnparsedFile -> State ParseState (Either ParseError ASM.Program)
runParseInstructionLines [] = return $ Right []
runParseInstructionLines (l:ls) = do
    parsedLine <- runParseInstructionLine l
    case parsedLine of
        (Right instr) -> do
                         instrs <- runParseInstructionLines ls
                         return $ (:) <$> Right instr <*> instrs
        (Left err)    -> return $ Left err

moveLabelsToSymbolTable :: S.UnparsedFile -> State ParseState S.UnparsedFile
moveLabelsToSymbolTable [] = return []
moveLabelsToSymbolTable (l:ls) = do
    st <- get
    case parseOnly parseAddressLabel (S.code l) of
        Right label -> do
                       insertToSymbolTable label (nextLineNumber st)
                       moveLabelsToSymbolTable ls
        Left  _     -> do
                       incNextLineNumber
                       ls' <- moveLabelsToSymbolTable ls
                       return $ l : ls'
                
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
parseAddressSymbol :: Parser String
parseAddressSymbol =
        skipSpace
    >>  char '@'
    >>  manyTill' anyChar   (lookAhead  (eitherP    endOfInput
                                                    (       char ' '
                                                        <|> char '/'
                                                        <|> char '\r'
                                                    )
                                        )
                            )
    <*  parseComment

-- Returns the label (as a string) from a label declaration
-- e.g. (LOOP), (END)
parseAddressLabel :: Parser String
parseAddressLabel =
        skipSpace
    >>  char '('
    >>  manyTill' anyChar (char ')') 
    <*  parseComment

-- ====================================================================================== --
-- C Instructions --

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

-- Combines the above parsers into one that deals with the full C instruction
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

-- ====================================================================================== --

-- Checks for comments, whitespace, or end of line, e.g. "   //this is a comment"
parseComment :: Parser ()
parseComment =
        skipSpace 
    >>  (   (string "//" >> return ())
        <|> endOfInput
        )

-- Checks for A instructions (without labels) and C instructions for one line of code
-- Also checks that the rest of the line is valid (i.e. contains only empty space, a comment,
-- or is the end of the line).
parseInstruction :: Parser ASM.Instruction
parseInstruction = 
        skipSpace 
    >>  (   parseAddressInstruction
        <|> parseComputationInstruction
        )
    <*  parseComment