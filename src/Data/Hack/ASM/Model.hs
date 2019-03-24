module Data.Hack.ASM.Model where
    
-- Data model for Hack instructions in ASM format.

import qualified Data.ByteString.Char8 as BS

data Computation =
    ZERO
  | ONE
  | MINUS_ONE
  | D_COMP
  | A_COMP
  | NOT_D
  | NOT_A
  | NEG_D
  | NEG_A
  | D_PLUS_1
  | A_PLUS_1
  | D_MINUS_1
  | A_MINUS_1
  | D_PLUS_A
  | D_MINUS_A
  | A_MINUS_D
  | D_AND_A
  | D_OR_A
  | M_COMP
  | NOT_M
  | NEG_M
  | M_PLUS_1
  | M_MINUS_1
  | D_PLUS_M
  | D_MINUS_M
  | M_MINUS_D
  | D_AND_M
  | D_OR_M

data Destination =
    NULL_DEST
  | M_DEST
  | D_DEST
  | MD
  | A_DEST
  | AM
  | AD
  | AMD

data Jump =
    NULL_JUMP
  | JGT
  | JEQ
  | JGE
  | JLT
  | JNE
  | JLE
  | JMP

type Address = Integer

data Instruction =
    AddressInstruction Address
  | ComputeInstruction Computation Destination Jump

type Program = [Line]

type ASMLineNumber =
    Integer

type ASMCode =
    BS.ByteString

data ASMLine =
    ASMLine ASMLineNumber ASMCode
    -- deriving (Eq, Show)

type HSLineNumber =
    Integer
data HSLine =
    HSLine HSLineNumber Instruction
    -- deriving (Show)

data Line =
    Line
    ASMLine
    HSLine
    -- deriving (Show)