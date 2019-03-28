module Data.Hack.MachineCode.Model where

data Line = Line { lineNumber :: Integer
                 , instruction :: Instruction }
type Program = [Line]
data File = File { program :: Program
                 , path    :: FilePath }

data Instruction =
        A { address     :: Integer}
    |   C { computation :: Integer
          , destination :: Integer
          , jump        :: Integer
          }

initLineNumber :: Integer
initLineNumber = 0