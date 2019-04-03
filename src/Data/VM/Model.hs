module Data.VM.Model where

import Control.Lens
import qualified Data.ByteString.Char8 as BS (ByteString)

data Line = Line { lineNumber :: Integer
                 , instruction :: Instruction }
type Program = [Line]
data File = File { program    :: [Line]
                 , path :: BS.ByteString }

data Instruction =
    AL_VM ArithLogicCommand
  | M_VM  MemoryAccessCommand
  | P_VM  ProgramFlowCommand
  | F_VM  FunctionCommand
  deriving (Eq, Show)

data ArithLogicCommand =
    ADD 
  | SUB
  | NEG
  | EQ_VM
  | GT_VM
  | LT_VM
  | AND
  | OR
  | NOT
  deriving (Eq, Show)

data MemoryAccessCommand =
    MemCMD  { direction :: Direction
            , segment   :: Segment
            , index     :: Integer }
            deriving (Eq, Show)

data Direction =
    PUSH
  | POP
  deriving (Eq, Show)

data Segment =
    ARGUMENT
  | LOCAL
  | STATIC
  | CONSTANT
  | THIS
  | THAT
  | POINTER
  | TEMP
  deriving (Eq, Show)

type Label = BS.ByteString

data ProgramFlowCommand =
    LABEL   Label
  | GOTO    Label
  | IF_GOTO Label
  deriving (Eq, Show)

data FunctionCommand =
    FUN  { label :: Label, nVars :: Integer }
  | CALL { label :: Label, nArgs :: Integer }
  | RETURN
  deriving (Eq, Show)