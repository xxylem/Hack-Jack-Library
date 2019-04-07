module Data.Hack.MachineCode.Model where

data Line = Line { lineNumber :: Integer
                 , instruction :: Instruction }
                 deriving (Eq, Show)
type Program = [Line]
data File = File { program :: Program
                 , path    :: FilePath }
                 deriving (Eq, Show)

data Instruction =
        A { address     :: Integer}
    |   C { computation :: Integer
          , destination :: Integer
          , jump        :: Integer
          }
          deriving (Eq, Show)

initLineNumber :: Integer
initLineNumber = 0