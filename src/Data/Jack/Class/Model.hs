module Data.Jack.Class.Model where

import qualified Data.ByteString.Char8 as BS (ByteString)

data File = File { jackClass :: JackClass
                 , path      :: FilePath }
                 deriving (Eq, Show)

newtype Identifier = Identifier BS.ByteString
                    deriving (Ord, Eq, Show)

data JackClass =
    JackClass { className       :: Identifier
              , classVarDecs    :: [ClassVarDec]
              , subroutineDecs  :: [SubroutineDec] }
              deriving (Eq, Show)

data ClassVarDec =
    ClassVarDec { classVarKind :: ClassVarKind
                , jackType     :: JackType
                , varNames     :: [Identifier] }
                deriving (Eq, Show)

data ClassVarKind =
    CVStatic
  | CVField
  deriving (Eq, Show)

data SubroutineDec =
    SubroutineDec { subroutineKind :: SubroutineKind
                  , subroutineType :: SubroutineType
                  , subroutineName :: Identifier
                  , parameters     :: [Parameter]
                  , subroutineBody :: SubroutineBody }
                    deriving (Eq, Show)

data SubroutineKind =
    SRConstructor
  | SRFunction
  | SRMethod
  deriving (Eq, Show)

data SubroutineType =
    VoidType
  | SRType JackType
  deriving (Eq, Show)

data Parameter =
    Param JackType Identifier
    deriving (Eq, Show)

data SubroutineBody =
    SubroutineBody [VarDec] [Statement]
    deriving (Eq, Show)

data VarDec =
    VarDec JackType [Identifier]
    deriving (Eq, Show)

data JackType =
    IntType
  | CharType
  | BoolType
  | ClassType Identifier
  deriving (Eq, Show)

data Statement =
    LetStatement LetStatementName Expression
  | IfStatement Expression [Statement] (Maybe [Statement])
  | WhileStatement Expression [Statement]
  | DoStatement SubroutineCall
  | ReturnStatement (Maybe Expression)
  deriving (Eq, Show)

data LetStatementName =
    LSV Identifier
  | LSA ArrayExp
  deriving (Eq, Show)

data Expression =
    ESingleTerm Term
  | ETermOpTerm { lTerm :: Term
                , op    :: Op
                , rTerm :: Term }
  deriving (Eq, Show)

data Term =
    TIntegerConstant Integer
  | TStringConstant BS.ByteString
  | TKeywordConstant EKeywordConstant
  | TVarName Identifier
  | TArrayExp ArrayExp
  | TSubroutineCall SubroutineCall
  | TParenExpression Expression
  | TUnaryOp UnaryOp Term
  deriving (Eq, Show)


data EKeywordConstant =
    EKConTrue
  | EKConFalse
  | EKConNull
  | EKConThis
  deriving (Eq, Show)

data ArrayExp =
    ArrayExp { arrayExpName :: Identifier
             , arrayExp     :: Expression }
    deriving (Eq, Show)

data SubroutineCall =
    SR      BasicSRCall
  | SRCN    { srcnClName :: Identifier
            , srcnSR     :: BasicSRCall }
  | SRVN    { srvnVName  :: Identifier
            , srvnSR     :: BasicSRCall }
  deriving (Eq, Show)

data BasicSRCall = BasicSRCall { srName :: Identifier
                               , srExpressions :: [Expression] }
                               deriving (Eq, Show)

data Op =
    OpPlus
  | OpMinus
  | OpMultiply
  | OpDivide
  | OpAnd
  | OpOr
  | OpLT
  | OpGT
  | OpEqual
  deriving (Eq, Show)

data UnaryOp =
    UOPArithNegation
  | UOPBitNegation
  deriving (Eq, Show)
