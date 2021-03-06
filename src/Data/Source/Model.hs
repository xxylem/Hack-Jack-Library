module Data.Source.Model where

import qualified Data.ByteString.Char8 as BS

data UnparsedLine =
    UnparsedLine { lineNumber :: Integer
                 , code       :: BS.ByteString }
                 deriving (Eq, Show)

type UnparsedProgram = [UnparsedLine]

data UnparsedFile =
    UnparsedFile { unparsedProgram :: UnparsedProgram
                 , path            :: FilePath }
                 deriving (Eq, Show)

toUnparsedFile :: FilePath -> Integer -> BS.ByteString -> UnparsedFile
toUnparsedFile fp n bs =
    -- Converts a ByteString to UnparsedFile format. Splits the ByteString by Line and
    -- numbers each line starting from n.
    UnparsedFile { unparsedProgram = go n (BS.lines bs) 
                 , path            = fp }
        where   go _ [] = []
                go n' (l:ls) = 
                    UnparsedLine { lineNumber=n', code = l } : go (n'+1) ls

data UnparsedBSFile = UnparsedBSFile { unparsedByteString :: BS.ByteString
                                     , bsPath    :: FilePath }

toUnparsedBSFile :: FilePath -> BS.ByteString -> UnparsedBSFile
toUnparsedBSFile fp bs = UnparsedBSFile { unparsedByteString = bs
                                        , bsPath               = fp }

