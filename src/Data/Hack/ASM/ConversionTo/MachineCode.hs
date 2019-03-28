{-# LANGUAGE BinaryLiterals #-}

module Data.Hack.ASM.ConversionTo.MachineCode (convert) where

import qualified Data.Hack.ASM.Model as ASM
import qualified Data.Hack.MachineCode.Model as MC

import System.FilePath (addExtension, dropExtension)

convert :: ASM.File -> MC.File
convert ASM.File { ASM.program = asmProg
                 , ASM.path    = asmPath } =
        MC.File { MC.program = go asmProg MC.initLineNumber
                , MC.path    = addExtension (dropExtension asmPath) "hack" } 
        where   go []     _   = []
                go (l:ls) lNum  =
                        MC.Line { MC.lineNumber    = lNum
                                , MC.instruction   = convertInstruction (ASM.instruction l) }
                        : go ls (lNum+1)

convertInstruction :: ASM.Instruction -> MC.Instruction
convertInstruction (ASM.A i) = MC.A i
convertInstruction ASM.C{ASM.computation = c, ASM.destination = d, ASM.jump = j} =
        MC.C {  MC.computation = convertComputation c
             ,  MC.destination = convertDestination d
             ,  MC.jump        = convertJump        j
             }

convertComputation :: ASM.Computation -> Integer
convertComputation cmp =
        case cmp of
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

convertDestination :: ASM.Destination -> Integer
convertDestination dest =
        case dest of
                ASM.NULL_DEST   -> 0b000
                ASM.M_DEST      -> 0b001
                ASM.D_DEST      -> 0b010
                ASM.MD          -> 0b011
                ASM.A_DEST      -> 0b100
                ASM.AM          -> 0b101
                ASM.AD          -> 0b110
                ASM.AMD         -> 0b111

convertJump :: ASM.Jump -> Integer
convertJump jump =
        case jump of
                ASM.NULL_JUMP   -> 0b000
                ASM.JGT         -> 0b001
                ASM.JEQ         -> 0b010
                ASM.JGE         -> 0b011
                ASM.JLT         -> 0b100
                ASM.JNE         -> 0b101
                ASM.JLE         -> 0b110
                ASM.JMP         -> 0b111