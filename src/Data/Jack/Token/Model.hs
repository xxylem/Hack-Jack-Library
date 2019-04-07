module Data.Jack.Token.Model where
    
import qualified Data.ByteString.Char8 as BS

data File = File { program :: [Token]
                 , path    :: FilePath }
                 deriving (Eq, Show)

data Keyword =
    Class
  | Constructor
  | Function
  | Method
  | Field
  | Static
  | Var
  | Int
  | Char
  | Boolean
  | Void
  | TrueKW
  | FalseKW
  | Null
  | This
  | Let
  | Do 
  | If
  | Else
  | While
  | Return
  deriving (Eq, Show)

data Symbol =
    LCurlyBracket
  | RCurlyBracket
  | LParen
  | RParen
  | LSquareBracket
  | RSquareBracket
  | FullStop
  | Comma
  | Semicolon
  | Plus
  | Minus
  | Asterix
  | Slash
  | Ampersand
  | VerticalBar
  | LAngleBracket
  | RAngleBracket
  | Equal
  | Tilde
  deriving (Eq, Show)

type IntegerConstant = Integer
type StringConstant = BS.ByteString
type Identifier = BS.ByteString

data Token =
    KW Keyword
  | SY Symbol
  | IC IntegerConstant
  | SC StringConstant
  | ID Identifier
  deriving (Eq, Show)