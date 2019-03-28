module Data.Output.Model where

import qualified Data.ByteString.Char8 as BS

data OutputFile =
    OutputFile { outputProgram   :: BS.ByteString
               , path            :: FilePath }

writeOutputFile :: OutputFile -> IO ()
writeOutputFile OutputFile { outputProgram = outProg
                           , path          = path } =
    BS.writeFile path outProg