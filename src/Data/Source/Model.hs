module Data.Source.Model where

import qualified Data.ByteString.Char8 as BS

type UnparsedFile =
    [UnparsedLine]

data UnparsedLine =
    UnparsedLine { lineNumber :: Integer
                 , code       :: BS.ByteString
                 }

toUnparsedFile :: Integer -> BS.ByteString -> UnparsedFile
toUnparsedFile n bs =
    -- Converts a ByteString to UnparsedFile format. Splits the ByteString by Line and
    -- numbers each line starting from n.
    go n (BS.lines bs)
    where go _ [] = []
          go n (l:ls) = UnparsedLine { lineNumber=n, code = l }
                        : go (n+1) ls