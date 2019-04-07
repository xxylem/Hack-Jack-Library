module Data.Output.Model where

import qualified Data.ByteString.Char8 as BS

data OutputFile =
    OutputFile { outputProgram   :: BS.ByteString
               , path            :: FilePath }
               deriving (Eq, Show)

writeOutputFiles :: [OutputFile] -> IO ()
writeOutputFiles [] = return ()
writeOutputFiles (f:fs) = do
    writeOutputFile f
    writeOutputFiles fs


writeOutputFile :: OutputFile -> IO ()
writeOutputFile OutputFile { outputProgram = outProg
                           , path          = path' } =
    BS.writeFile path' outProg