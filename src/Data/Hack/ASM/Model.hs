module Data.Hack.ASM.Model where
  
import qualified Data.ByteString.Char8 as BS (ByteString)


-- Data model for Hack instructions in ASM format.

data Line = Line { lineNumber :: Integer
                 , instruction :: Instruction }
type Program = [Line]
data File = File { program :: Program
                 , path    :: FilePath }


data Instruction =
    A { address     :: Integer}
  | AL { addressLabel :: BS.ByteString }
  | C { computation :: Computation
      , destination :: Destination
      , jump        :: Jump }

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
