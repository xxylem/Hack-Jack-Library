{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Data.Hack.ASM.ConversionTo.ByteString where

import qualified Data.Hack.ASM.Model as ASM

import Data.ByteString.Conversion (toByteString')
import qualified Data.ByteString.Char8 as BS (ByteString)

class Convert a where
    convert :: a -> BS.ByteString

instance Convert ASM.Program where
    convert [] = ""
    convert (l:ls) = convert l <> "\n" <> convert ls

instance Convert ASM.Line where
    convert line = convert $ ASM.instruction line

instance Convert ASM.Instruction where
    convert (ASM.A i) = "@" <> toByteString' i
    convert ASM.C{ ASM.computation = c
                 , ASM.destination = d
                 , ASM.jump        = j } = 
                    undefined
