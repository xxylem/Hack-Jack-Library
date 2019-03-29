module Parser.Error where

import qualified Data.Source.Model as S

data ErrorType =
        InvalidLine
    |   NoSymbol
    deriving (Eq, Show)

data ParseError = 
    ParseError  { errorType :: ErrorType
                , message   :: String
                , line      :: S.UnparsedLine }
                deriving (Eq, Show)