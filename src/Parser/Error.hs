module Parser.Error where

import qualified Data.Source.Model as S

data ParseError = 
    ParseError  { message   :: String
                , line      :: S.UnparsedLine }
                deriving (Eq, Show)