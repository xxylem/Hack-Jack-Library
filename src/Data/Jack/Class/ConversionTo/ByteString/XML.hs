{-# LANGUAGE OverloadedStrings #-}

module Data.Jack.Class.ConversionTo.ByteString.XML where

import qualified Data.Jack.Class.Model as JC
import qualified Data.Output.Model as OUT

import qualified Data.ByteString.Char8 as BS
import System.FilePath (addExtension, dropExtension)

convertDirectory :: [JC.File] -> [OUT.OutputFile]
convertDirectory = map convertSingleFile

convertSingleFile :: JC.File -> OUT.OutputFile
convertSingleFile JC.File { JC.jackClass = jClass
                          , JC.path      = jcPath } =
    OUT.OutputFile { OUT.outputProgram = toXML jClass 
                   , OUT.path = addExtension (dropExtension jcPath) "xml"}

identifierXML :: BS.ByteString -> BS.ByteString
identifierXML i =
    "<identifier> " <> i <> " </identifier>\n"

keywordXML :: BS.ByteString -> BS.ByteString
keywordXML kw =
    "<keyword> " <> kw <> " </keyword>\n"

symbolXML :: BS.ByteString -> BS.ByteString
symbolXML sym =
    "<symbol> " <> sym <> " </symbol>\n"

class ToXML a where
    toXML :: a -> BS.ByteString

instance ToXML JC.Identifier where
    toXML (JC.Identifier i) = identifierXML i

instance ToXML JC.JackClass where
    toXML (JC.JackClass cName cvDecs srDecs) =
            "<class>\n"
        <>  keywordXML "class"
        <>  toXML cName
        <>  symbolXML "{"
        <>  foldr ((<>) . toXML) "" cvDecs
        <>  foldr ((<>) . toXML) "" srDecs
        <>  symbolXML "}"
        <>  " </class>\n"

instance ToXML JC.ClassVarDec where
    toXML (JC.ClassVarDec cvKind jType vNames) =
            "<classVarDec>\n"
        <>  toXML cvKind
        <>  toXML jType
        <>  varNameListXML vNames
        <>  symbolXML ";"
        <>  " </classVarDec>\n"

instance ToXML JC.ClassVarKind where
    toXML JC.CVStatic = keywordXML "static"
    toXML JC.CVField = keywordXML "field"

instance ToXML JC.SubroutineDec where
    toXML (JC.SubroutineDec sKind
                         sType
                         sName
                         params
                         sBody) =
            "<subroutineDec>\n"
        <>  toXML sKind
        <>  toXML sType
        <>  toXML sName
        <>  symbolXML "("
        <>  paramListXML params
        <>  symbolXML ")"
        <>  toXML sBody
        <>  " </subroutineDec>\n"

instance ToXML JC.SubroutineKind where
    toXML JC.SRConstructor = keywordXML "constructor"
    toXML JC.SRFunction = keywordXML "function"
    toXML JC.SRMethod = keywordXML "method"
    
instance ToXML JC.SubroutineType where
    toXML JC.VoidType = keywordXML "void"
    toXML (JC.SRType jType) = toXML jType

paramListXML :: [JC.Parameter] -> BS.ByteString
paramListXML ps =
        "<parameterList>\n"
    <>  go ps
    <>  " </parameterList>\n"
        where go [] = ""
              go (p:ps) =
                toXML p <> go' ps
              go' [] = ""
              go' (p':ps') =
                    symbolXML ","
                <>  toXML p'
                <>  go' ps'
        

instance ToXML JC.Parameter where
    toXML (JC.Param jType vName) =
            toXML jType
        <>  toXML vName

instance ToXML JC.SubroutineBody where
    toXML (JC.SubroutineBody varDecs stmts) =
            "<subroutineBody>\n"
        <>  symbolXML "{"
        <>  varDecListXML varDecs
        <>  statementListXML stmts
        <>  symbolXML "}"
        <>  " </subroutineBody>\n"

varDecListXML :: [JC.VarDec] -> BS.ByteString
varDecListXML = foldr ((<>) . toXML) ""
        
instance ToXML JC.VarDec where
    toXML (JC.VarDec jType vNames) =
            "<varDec>\n"
        <>  keywordXML "var"
        <>  toXML jType
        <>  varNameListXML vNames
        <>  symbolXML ";"
        <>  " </varDec>\n"

varNameListXML :: [JC.Identifier] -> BS.ByteString
varNameListXML [] = ""
varNameListXML (v:vs) =
    toXML v <> go vs
        where go [] = ""
              go (v':vs') =
                    symbolXML ","
                <>  toXML v'
                <>  go vs'

instance ToXML JC.JackType where
    toXML JC.IntType = keywordXML "int"
    toXML JC.CharType = keywordXML "char"
    toXML JC.BoolType = keywordXML "boolean"
    toXML (JC.ClassType clName) = toXML clName

instance ToXML JC.Statement where
    toXML (JC.LetStatement lsName expr) =
            "<letStatement>\n"
        <>  keywordXML "let"
        <>  toXML lsName
        <>  symbolXML "="
        <>  toXML expr
        <>  symbolXML ";"
        <>  " </letStatement>\n"
    toXML (JC.IfStatement expr stmts elseStmts) =
            "<ifStatement>\n"
        <>  keywordXML "if"
        <>  symbolXML "("
        <>  toXML expr
        <>  symbolXML ")"
        <>  symbolXML "{"
        <>  statementListXML stmts
        <>  symbolXML "}"
        <>  (case elseStmts of
            Just ss -> keywordXML "else"
                    <> symbolXML "{"
                    <> statementListXML ss
                    <> symbolXML "}"
            Nothing -> "")
        <>  " </ifStatement>\n"
    toXML (JC.WhileStatement expr stmts) =
            "<whileStatement>\n"
        <>  keywordXML "while"
        <>  symbolXML "("
        <>  toXML expr
        <>  symbolXML ")"
        <>  symbolXML "{"
        <>  statementListXML stmts
        <>  symbolXML "}"
        <>  " </whileStatement>\n"
    toXML (JC.DoStatement srCall) =
            "<doStatement>\n"
        <>  keywordXML "do"
        <>  toXML srCall
        <>  symbolXML ";"
        <>  " </doStatement>\n"
    toXML (JC.ReturnStatement mbExpr) =
            "<returnStatement>\n"
        <>  keywordXML "return"
        <>  (case mbExpr of
            Just expr -> toXML expr
            Nothing   -> "")
        <>  symbolXML ";"
        <>  " </returnStatement>\n"


statementListXML :: [JC.Statement] -> BS.ByteString
statementListXML stmts =
        "<statements>\n"
    <>  foldr ((<>) . toXML) "" stmts
    <>  " </statements>\n"

instance ToXML JC.LetStatementName where
    toXML (JC.LSV vName) = toXML vName
    toXML (JC.LSA eArrExp) = toXML eArrExp

instance ToXML JC.Expression where
    toXML e =
            "<expression>\n"
        <>  (case e of
            JC.ESingleTerm t -> toXML t
            JC.ETermOpTerm t1 op t2 ->
                    toXML t1
                <>  toXML op
                <>  toXML t2)
        <>  " </expression>\n"

instance ToXML JC.Term where
    toXML term =
            "<term>\n"
        <>  (case term of
            JC.TIntegerConstant i ->
                    "<integerConstant>"
                <>  (BS.pack . show) i
                <>  " </integerConstant>\n"
            JC.TStringConstant s ->
                    "<stringConstant>"
                <>  s
                <>  " </stringConstant>\n"
            JC.TKeywordConstant kw ->
                toXML kw
            JC.TVarName varName ->
                toXML varName
            JC.TArrayExp eArrExp ->
                toXML eArrExp
            JC.TSubroutineCall sc ->
                toXML sc
            JC.TParenExpression expr ->
                    symbolXML "("
                <>  toXML expr
                <>  symbolXML ")"
            JC.TUnaryOp uop uoTerm ->
                    toXML uop
                <>  toXML uoTerm)
        <>  " </term>\n"

instance ToXML JC.EKeywordConstant where
    toXML JC.EKConTrue = keywordXML "true"
    toXML JC.EKConFalse = keywordXML "false"
    toXML JC.EKConNull = keywordXML "null"
    toXML JC.EKConThis = keywordXML "this"

instance ToXML JC.ArrayExp where
    toXML (JC.ArrayExp varName expr) =
            toXML varName
        <>  symbolXML "["
        <>  toXML expr
        <>  symbolXML "]"


instance ToXML JC.SubroutineCall where
    toXML (JC.SR srCall ) =
            toXML srCall
    toXML (JC.SRCN clName srCall) =
            toXML clName
        <>  symbolXML "."
        <>  toXML srCall
    toXML (JC.SRVN vName srCall) =
            toXML vName
        <>  symbolXML "."
        <>  toXML srCall

instance ToXML JC.BasicSRCall where
        toXML JC.BasicSRCall { JC.srName = srName'
                             , JC.srExpressions = srExpressions' } =
                toXML srName'
            <>  symbolXML "("
            <>  expressionListXML srExpressions'
            <>  symbolXML ")"

expressionListXML :: [JC.Expression] -> BS.ByteString
expressionListXML exprs =
        "<expressionList>\n"
    <>  go exprs
    <>  " </expressionList>\n"
    where go [] = ""
          go (e:es) =
            toXML e <> go' es
          go' [] = ""
          go' (e':es') =
                symbolXML "," 
            <>  toXML e'
            <>  go' es'

instance ToXML JC.Op where
    toXML JC.OpPlus = symbolXML "+"
    toXML JC.OpMinus = symbolXML "-"
    toXML JC.OpMultiply = symbolXML "*"
    toXML JC.OpDivide = symbolXML "/"
    toXML JC.OpAnd = symbolXML "&amp;"
    toXML JC.OpOr = symbolXML "|"
    toXML JC.OpLT = symbolXML "&lt;"
    toXML JC.OpGT = symbolXML "&gt;"
    toXML JC.OpEqual = symbolXML "="

instance ToXML JC.UnaryOp where
    toXML JC.UOPArithNegation = symbolXML "-"
    toXML JC.UOPBitNegation = symbolXML "~"