{-# LANGUAGE OverloadedStrings #-}

module Data.Hack.MachineCode.ConversionTo.ByteString (convert) where

import Data.Hack.MachineCode.Model (HackFile)

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Conversion (toByteString')


convert :: HackFile -> BS.ByteString
convert [] = ""
convert (i:is) = toByteString' i <> "\n" <> convert is