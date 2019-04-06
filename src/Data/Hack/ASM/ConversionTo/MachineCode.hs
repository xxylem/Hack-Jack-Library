{-# LANGUAGE BinaryLiterals, OverloadedStrings #-}

module Data.Hack.ASM.ConversionTo.MachineCode (convert) where

import qualified Data.Hack.ASM.Model as ASM
import qualified Data.Hack.MachineCode.Model as MC

import Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as BS (ByteString)
import qualified Data.Map.Strict as Map
import System.FilePath (addExtension, dropExtension)

-- ========================= --
-- File Processing Functions --
-- ========================= --

convert :: ASM.File -> MC.File
convert ASM.File { ASM.program = asmProg
                                , ASM.path    = asmPath } =
    let (noLabels, st) = runState (processLabels asmProg) initState
        mcLines        = evalState (convertLines noLabels) st
    in MC.File { MC.program = mcLines
                , MC.path    = addExtension (dropExtension asmPath) "hack" }

-- ASSUMES: Line numbers are correct due to preprocessing by processLabels.
convertLines :: [ASM.Line] -> State ConversionState [MC.Line]
convertLines [] = return []
convertLines (ASM.Line { ASM.lineNumber = asmLN
                      , ASM.instruction = asmIn } : is) = do
        i' <- convertInstruction asmIn
        is' <- convertLines is
        return (MC.Line { MC.lineNumber = asmLN
                        , MC.instruction = i' } : is')

-- Resolves (labels) and puts them in the symbol table, building up a new list
-- of ASM lines.
processLabels :: [ASM.Line] -> State ConversionState [ASM.Line]
processLabels [] = return []
processLabels (ASM.Line { ASM.instruction = i } : is) = do
    st <- get
    let lineNum = nextLineNumber st
    case i of
      ASM.L { ASM.label = lb } -> do
                             put $ st { symbolTable = Map.insert lb lineNum (symbolTable st) }
                             processLabels is
      _                   -> do
                             put $ st { nextLineNumber = lineNum + 1 }
                             is' <- processLabels is
                             return $ ( ASM.Line { ASM.lineNumber = lineNum 
                                                 , ASM.instruction = i } ) : is'

-- ================================ --
-- Instruction Conversion Functions
-- ================================ --

convertInstruction :: ASM.Instruction -> State ConversionState MC.Instruction
convertInstruction (ASM.A i) = return $ MC.A i

convertInstruction ASM.C { ASM.computation = c
                         , ASM.destination = d
                         , ASM.jump = j } = do
        c' <- convertComputation c
        d' <- convertDestination d
        j' <- convertJump j
        return $ MC.C   {  MC.computation = c'
                        ,  MC.destination = d'
                        ,  MC.jump        = j' }

convertInstruction ASM.S { ASM.addressSymbol = sym } = do
                st <- get
                let symTab = symbolTable st
                case Map.lookup sym symTab of
                        Just val -> return $ MC.A val
                        Nothing  -> do
                                let aVal = nextAddressValue st
                                put $ st { nextAddressValue = aVal + 1
                                         , symbolTable = Map.insert sym aVal symTab }
                                return $ MC.A aVal

convertInstruction ASM.L {} = error "labels are all removed by this point"

convertComputation :: ASM.Computation -> State ConversionState Integer
convertComputation cmp =
        return $ case cmp of
            ASM.ZERO        -> 0b0101010
            ASM.ONE         -> 0b0111111
            ASM.MINUS_ONE   -> 0b0111010
            ASM.D_COMP      -> 0b0001100
            ASM.A_COMP      -> 0b0110000
            ASM.NOT_D       -> 0b0001101
            ASM.NOT_A       -> 0b0110001
            ASM.NEG_D       -> 0b0001111
            ASM.NEG_A       -> 0b0110001
            ASM.D_PLUS_1    -> 0b0011111
            ASM.A_PLUS_1    -> 0b0110111
            ASM.D_MINUS_1   -> 0b0001110
            ASM.A_MINUS_1   -> 0b0110010
            ASM.D_PLUS_A    -> 0b0000010
            ASM.D_MINUS_A   -> 0b0010011
            ASM.A_MINUS_D   -> 0b0000111
            ASM.D_AND_A     -> 0b0000000
            ASM.D_OR_A      -> 0b0010101
            ASM.M_COMP      -> 0b1110000
            ASM.NOT_M       -> 0b1110001
            ASM.NEG_M       -> 0b1110011
            ASM.M_PLUS_1    -> 0b1110111
            ASM.M_MINUS_1   -> 0b1110010
            ASM.D_PLUS_M    -> 0b1000010
            ASM.D_MINUS_M   -> 0b1010011
            ASM.M_MINUS_D   -> 0b1000111
            ASM.D_AND_M     -> 0b1000000
            ASM.D_OR_M      -> 0b1010101

convertDestination :: ASM.Destination -> State ConversionState Integer
convertDestination dest =
        return $ case dest of
                ASM.NULL_DEST   -> 0b000
                ASM.M_DEST      -> 0b001
                ASM.D_DEST      -> 0b010
                ASM.MD          -> 0b011
                ASM.A_DEST      -> 0b100
                ASM.AM          -> 0b101
                ASM.AD          -> 0b110
                ASM.AMD         -> 0b111

convertJump :: ASM.Jump -> State ConversionState Integer
convertJump jump =
        return $ case jump of
                ASM.NULL_JUMP   -> 0b000
                ASM.JGT         -> 0b001
                ASM.JEQ         -> 0b010
                ASM.JGE         -> 0b011
                ASM.JLT         -> 0b100
                ASM.JNE         -> 0b101
                ASM.JLE         -> 0b110
                ASM.JMP         -> 0b111


-- ============================================= --
-- Conversion State Datatypes and Initialisation --
-- ============================================= --
data ConversionState = ConversionState  { nextAddressValue :: Integer 
                                        , nextLineNumber   :: Integer
                                        , symbolTable :: SymbolTable }

type AddressVal = Integer
type SymbolTable = Map.Map BS.ByteString AddressVal

initState :: ConversionState
initState = ConversionState { nextAddressValue = 16
                            , nextLineNumber   = 0
                            , symbolTable = initSymbolTable }

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
                 ,  ("KBD",     24576) ]                 
