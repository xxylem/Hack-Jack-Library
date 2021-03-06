module Data.Hack.ASM.Model where
  
import qualified Data.ByteString.Char8 as BS (ByteString)
import System.FilePath (addExtension, dropExtension)

-- Data model for Hack instructions in ASM format.

data Line = Line { lineNumber :: Integer
                 , instruction :: Instruction }
                 deriving (Eq, Show)
type Program = [Line]
data File = File { program :: Program
                 , path    :: FilePath }
                 deriving (Eq, Show)

makeFileFromInstructions :: FilePath -> [Instruction] -> File
makeFileFromInstructions fp is =
  File { program = makeLines is
       , path    = addExtension (dropExtension fp) "asm" }

makeLines :: [Instruction] -> [Line]
makeLines = go 0
  where go _ [] = []
        go lNum (i':is') =
          Line { lineNumber = lNum
               , instruction = i'} : go (lNum + 1) is'

data Instruction =
    A { address       :: Integer}
  | C { computation   :: Computation
      , destination   :: Destination
      , jump          :: Jump }
  | L { label         :: BS.ByteString}
  | S { addressSymbol :: BS.ByteString }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data Destination =
    NULL_DEST
  | M_DEST
  | D_DEST
  | MD
  | A_DEST
  | AM
  | AD
  | AMD
  deriving (Eq, Show)

data Jump =
    NULL_JUMP
  | JGT
  | JEQ
  | JGE
  | JLT
  | JNE
  | JLE
  | JMP
  deriving (Eq, Show)
