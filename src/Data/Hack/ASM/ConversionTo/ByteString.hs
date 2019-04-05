{-# LANGUAGE OverloadedStrings #-}

module Data.Hack.ASM.ConversionTo.ByteString where

import qualified Data.Hack.ASM.Model as ASM
import qualified Data.Output.Model as OUT

import qualified Data.ByteString.Char8 as BS (ByteString, pack)

convert :: ASM.File -> OUT.OutputFile
convert ASM.File { ASM.program = asmProg
                 , ASM.path    = asmPath } =
    OUT.OutputFile { OUT.outputProgram = go asmProg 
                   , OUT.path          = asmPath }
    where   go [] = ""
            go (l:ls) = convertLine l <> "\n" <> go ls

convertLine :: ASM.Line -> BS.ByteString
convertLine ASM.Line { ASM.instruction = asmIn } = go asmIn
    where   go (ASM.A i) = "@" <> (BS.pack . show) i
            go ASM.C{ ASM.computation = c
                    , ASM.destination = d
                    , ASM.jump        = j } = 
                            convertComputation c
                        <>  convertDestination d
                        <>  convertJump j

convertComputation :: ASM.Computation -> BS.ByteString
convertComputation c =
    case c of
        ASM.ZERO        -> "0"
        ASM.ONE         -> "1"
        ASM.MINUS_ONE   -> "-1"
        ASM.D_COMP      -> "D"
        ASM.A_COMP      -> "A"
        ASM.NOT_D       -> "!D"
        ASM.NOT_A       -> "!A"
        ASM.NEG_D       -> "-D"
        ASM.NEG_A       -> "-A"
        ASM.D_PLUS_1    -> "D+1" 
        ASM.A_PLUS_1    -> "A+1"
        ASM.D_MINUS_1   -> "D-1"
        ASM.A_MINUS_1   -> "A-1" 
        ASM.D_PLUS_A    -> "D+A"
        ASM.D_MINUS_A   -> "D-A" 
        ASM.A_MINUS_D   -> "A-D" 
        ASM.D_AND_A     -> "D&A"
        ASM.D_OR_A      -> "D|A"
        ASM.M_COMP      -> "M"
        ASM.NOT_M       -> "!M"
        ASM.NEG_M       -> "-M" 
        ASM.M_PLUS_1    -> "M+1"
        ASM.M_MINUS_1   -> "M-1"
        ASM.D_PLUS_M    -> "D+M"
        ASM.D_MINUS_M   -> "D-M"
        ASM.M_MINUS_D   -> "M-D"
        ASM.D_AND_M     -> "D&M"
        ASM.D_OR_M      -> "D|M"

convertDestination :: ASM.Destination -> BS.ByteString
convertDestination ASM.NULL_DEST = ""
convertDestination d =
    (case d of
        ASM.M_DEST  -> "M"
        ASM.D_DEST  -> "D"
        ASM.MD      -> "MD"
        ASM.A_DEST  -> "A"
        ASM.AM      -> "AM"
        ASM.AD      -> "AD"
        ASM.AMD     -> "AMD"
        ASM.NULL_DEST -> error "impossible program state reached")
    <> "="

convertJump :: ASM.Jump -> BS.ByteString
convertJump ASM.NULL_JUMP = ""
convertJump j =
    ";" <>
    (case j of
        ASM.JGT -> "JGT"
        ASM.JEQ -> "JEQ"
        ASM.JGE -> "JGE"
        ASM.JLT -> "JLT"
        ASM.JNE -> "JNE"
        ASM.JLE -> "JLE"
        ASM.JMP -> "JMP"
        ASM.NULL_JUMP -> error "impossible program state reached")