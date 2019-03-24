{-# LANGUAGE OverloadedStrings #-}

module Data.Hack.MachineCode.ConversionTo.ByteString (convert) where

import qualified Data.Hack.MachineCode.Model as MC

import qualified Data.ByteString.Char8 as BS (ByteString)
import Data.ByteString.Conversion (toByteString')


convert :: MC.HackFile -> BS.ByteString
convert [] = ""
convert (l:ls) = toByteString' (MC.instruction l) <> "\n" <> convert ls