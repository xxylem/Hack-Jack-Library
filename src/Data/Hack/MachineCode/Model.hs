module Data.Hack.MachineCode.Model where

data HackLine = HackLine { lineNumber :: Integer
                         , instruction :: Instruction
                         }
type HackFile = [HackLine]

data Instruction =
        A { address     :: Integer}
    |   C { computation :: Integer
          , destination :: Integer
          , jump        :: Integer
          }