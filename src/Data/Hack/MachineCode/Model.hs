module Data.Hack.MachineCode.Model where

data HackLine = HackLine { lineNumber :: Integer
                         , instruction :: Integer
                         }
type HackFile = [HackLine]