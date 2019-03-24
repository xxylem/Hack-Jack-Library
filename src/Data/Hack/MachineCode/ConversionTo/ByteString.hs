{-# LANGUAGE OverloadedStrings, BinaryLiterals #-}

module Data.Hack.MachineCode.ConversionTo.ByteString (convert) where

import qualified Data.Hack.MachineCode.Model as MC

import qualified Data.ByteString.Char8 as BS (ByteString)
import Data.ByteString.Conversion (toByteString')
import Data.Char (intToDigit)
import Text.Show.ByteString (runPut, showpIntAtBase)


convert :: MC.HackFile -> BS.ByteString
convert [] = ""
convert (l:ls) =    convertInstruction (MC.instruction l)
               <>   convert ls
               
convertInstruction :: MC.Instruction -> BS.ByteString
convertInstruction (MC.A i) = undefined
convertInstruction MC.C{ MC.computation = c
                       , MC.destination = d
                       , MC.jump        = j } =
        runPut $
        showpIntAtBase 2 intToDigit 0b111
    <>  showpIntAtBase 2 intToDigit c
    <>  showpIntAtBase 2 intToDigit d
    <>  showpIntAtBase 2 intToDigit j 
    
    
