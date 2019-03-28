{-# LANGUAGE OverloadedStrings, BinaryLiterals #-}

module Data.Hack.MachineCode.ConversionTo.ByteString (convert) where

import qualified Data.Hack.MachineCode.Model as MC
import qualified Data.Output.Model as OUT

import qualified Data.ByteString.Char8 as BS (ByteString, pack)
import Text.Printf (printf)


convert :: MC.File -> OUT.OutputFile
convert MC.File { MC.program = mcProg
                , MC.path    = mcPath} =
    OUT.OutputFile { OUT.outputProgram = go mcProg
                   , OUT.path          = mcPath }
    where   go [] = ""
            go (l:ls) =    convertInstruction (MC.instruction l)
                        <>   "\n"
                        <>   go ls
               
convertInstruction :: MC.Instruction -> BS.ByteString
                            -- prints out address as 16bit binary, first bit always 0
convertInstruction (MC.A i) = BS.pack $ printf "%016b" i 
convertInstruction MC.C{ MC.computation = c
                       , MC.destination = d
                       , MC.jump        = j } =
    BS.pack $ 
        -- prints out components of C instruction in binary format with leading zeroes
        printf "%07b" c 
    <>  printf "%03b" d
    <>  printf "%03b" j
