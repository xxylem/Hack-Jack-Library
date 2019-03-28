{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Data.Hack.ASM.ConversionTo.ByteString where

import qualified Data.Hack.ASM.Model as ASM
import qualified Data.Output.Model as OUT

import qualified Data.ByteString.Char8 as BS (ByteString, pack)

convert :: ASM.File -> OUT.OutputFile
convert ASM.File { ASM.program = asmProg
                 , ASM.path    = asmPath } =
    OUT.OutputFile { OUT.outputProgram = go asmProg 
                   , OUT.path          = asmPath }
    where   go [] = ""
            go (l:ls) = convertLine l <> "\n" <> go ls

convertLine :: ASM.Line -> BS.ByteString
convertLine ASM.Line { ASM.instruction = asmIn } = go asmIn
    where   go (ASM.A i) = "@" <> (BS.pack . show) i
            go ASM.C{ ASM.computation = c
                    , ASM.destination = d
                    , ASM.jump        = j } = 
                        undefined
