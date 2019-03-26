{-# LANGUAGE OverloadedStrings, BinaryLiterals #-}

module Data.Hack.MachineCode.ConversionTo.ByteString (convert) where

import qualified Data.Hack.MachineCode.Model as MC

import qualified Data.ByteString.Char8 as BS (ByteString, pack)
import Text.Printf (printf)


convert :: MC.HackFile -> BS.ByteString
convert [] = ""
convert (l:ls) =    convertInstruction (MC.instruction l)
               <>   "\n"
               <>   convert ls
               
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
