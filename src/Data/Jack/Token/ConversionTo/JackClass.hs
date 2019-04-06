{-# LANGUAGE FlexibleInstances #-}

module Data.Jack.Token.ConversionTo.JackClass where

import qualified Data.Jack.Token.Model as TK
import qualified Data.Jack.Class.Model as JC


import Control.Applicative
import Data.Char (isUpper, isLower)
import qualified Data.ByteString.Char8 as BS

type TokenParser a =
        [TK.Token]
    ->  Either (String, [TK.Token]) (a, [TK.Token])

runParseJackClasses :: [TK.File] -> Either (String, [TK.Token]) [JC.File]
runParseJackClasses [] = Right []
runParseJackClasses (TK.File { TK.program = tkProg
                             , TK.path    = path}:fs) = do
    jackClass <- runParseJackClass tkProg
    jackClasses <- runParseJackClasses fs
    return (JC.File { JC.jackClass = jackClass
                    , JC.path      = path} :jackClasses)

runParseJackClass :: [TK.Token] -> Either (String, [TK.Token]) JC.JackClass
runParseJackClass f =
        parseJackClass f
    >>= \(jackClass, _)
    ->  return jackClass

parseJackClass :: TokenParser JC.JackClass
parseJackClass ts =
        skipClass ts
    >>= \(_, ts)
    ->  parseIdentifier ts
    >>= \(cName, ts)
    ->  skipLCurlyBracket ts
    >>= \(_, ts)
    ->  parseClassVarDecs ts
    >>= \(cVarDecs, ts)
    ->  parseSubroutineDecs ts
    >>= \(srDecs, ts)
    ->  skipRCurlyBracket ts
    >>= \(_, ts)
    ->  checkEndOfInput ts
    >>= \(_, ts)
    ->  return (JC.JackClass cName
                          cVarDecs
                          srDecs, ts)

checkEndOfInput :: TokenParser ()
checkEndOfInput [] = Right ((), [])
checkEndOfInput ts = Left ("should be end of file", ts)

parseClassVarDecs :: TokenParser [JC.ClassVarDec]
parseClassVarDecs ts =
    case parseClassVarDec ts of
        Right (cVarDec, ts) -> case parseClassVarDecs ts of
                                Right (cVarDecs, ts') -> Right (cVarDec:cVarDecs, ts')
                                Left _              -> Right ([cVarDec], ts)
        Left _            -> Right ([], ts)

parseClassVarDec :: TokenParser JC.ClassVarDec
parseClassVarDec ts =
        parseClassVarKind ts
    >>= \(cVarKind, ts)
    ->  parseJackType ts
    >>= \(jType, ts)
    ->  parseIdentifierList ts
    >>= \(vNames, ts)
    ->  skipSemicolon ts
    >>= \(_, ts)
    ->  return (JC.ClassVarDec cVarKind
                            jType
                            vNames,
                            ts)

parseClassVarKind :: TokenParser JC.ClassVarKind
parseClassVarKind ts =
        (   skipStatic ts
        >>= \(_, ts)
        ->  return (JC.CVStatic, ts))
    <|> (   skipField ts
        >>= \(_, ts)
        ->  return (JC.CVField, ts))

parseJackType :: TokenParser JC.JackType
parseJackType ts =
        (   skipIntKw ts
        >>= \(_, ts)
        ->  return (JC.IntType, ts))
    <|> (   skipCharKw ts
        >>= \(_, ts)
        ->  return (JC.CharType, ts))
    <|> (   skipBoolKw ts
        >>= \(_, ts)
        ->  return (JC.BoolType, ts))
    <|> (   parseIdentifier ts
        >>= \(className, ts)
        ->  return (JC.ClassType className, ts))

parseSubroutineDecs :: TokenParser [JC.SubroutineDec]
parseSubroutineDecs ts =
    case parseSubroutineDec ts of
        Right (srDec, ts) -> case parseSubroutineDecs ts of
                                Right (srDecs, ts') -> Right (srDec:srDecs, ts')
                                Left _            -> Right ([srDec], ts)
        Left _          -> Right ([], ts)

parseSubroutineDec :: TokenParser JC.SubroutineDec
parseSubroutineDec ts =
        parseSubroutineKind ts
    >>= \(srKind, ts)
    ->  parseSubroutineType ts
    >>= \(srType, ts)
    ->  parseIdentifier ts
    >>= \(srName, ts)
    ->  skipLParen ts
    >>= \(_, ts)
    ->  parseParameters ts
    >>= \(params, ts)
    ->  skipRParen ts
    >>= \(_, ts)
    ->  parseSubroutineBody ts
    >>= \(body, ts)
    ->  return (JC.SubroutineDec srKind
                              srType
                              srName
                              params
                              body,  ts)

parseSubroutineKind :: TokenParser JC.SubroutineKind
parseSubroutineKind ts =
        (   skipConstructor ts
        >>= \(_, ts)
        ->  return (JC.SRConstructor, ts))
    <|> (   skipFunction ts
        >>= \(_, ts)
        ->  return (JC.SRFunction, ts))
    <|> (   skipMethod ts
        >>= \(_, ts)
        ->  return (JC.SRMethod, ts))

parseSubroutineType :: TokenParser JC.SubroutineType
parseSubroutineType ts =
        (   skipVoid ts
        >>= \(_, ts)
        ->  return (JC.VoidType, ts))
    <|> (case parseJackType ts of
            Right (jType, ts) -> Right (JC.SRType jType, ts)
            Left (err, errTkns)          -> Left (err <> " parseSubroutineType ", errTkns))

parseParameters :: TokenParser [JC.Parameter]
parseParameters ts =
        case parseFirstParameter ts of
            Right (prm, ts) ->  parseTailParameters ts
                                        >>= \(prms, ts)
                                        ->  return (prm:prms, ts)
            Left _ -> return ([], ts)


parseTailParameters :: TokenParser [JC.Parameter]
parseTailParameters ts =
    case parseTailParameter ts of
        Right (prm, ts) -> case parseTailParameters ts of
                            Right (prms, ts') -> Right (prm:prms, ts')
                            Left _          -> Right ([prm], ts)
        Left _        -> Right ([], ts)

parseTailParameter :: TokenParser JC.Parameter
parseTailParameter ts =
        skipComma ts
    >>= \(_, ts)
    ->  parseFirstParameter ts

parseFirstParameter :: TokenParser JC.Parameter
parseFirstParameter ts =
        parseJackType ts
    >>= \(jackType, ts)
    ->  parseIdentifier ts
    >>= \(varName, ts)
    ->  return (JC.Param jackType varName, ts)

parseSubroutineBody :: TokenParser JC.SubroutineBody
parseSubroutineBody ts =
        skipLCurlyBracket ts
    >>= \(_, ts)
    ->  parseVarDecs ts
    >>= \(varDecs, ts)
    ->  parseStatements ts
    >>= \(stmts, ts)
    ->  skipRCurlyBracket ts
    >>= \(_, ts)
    ->  return (JC.SubroutineBody varDecs stmts, ts)

parseIdentifierListOption :: TokenParser JC.Identifier
parseIdentifierListOption ts =
        skipComma ts
    >>= \(_, ts)
    ->  parseIdentifier ts

parseIdentifierListOptions :: TokenParser [JC.Identifier]
parseIdentifierListOptions ts =
        case parseIdentifierListOption ts of
            Right (name, ts) -> case parseIdentifierListOptions ts of
                                    Right (names, ts') -> Right (name:names, ts')
                                    Left _           -> Right ([name], ts)
            Left _         -> Right ([], ts)

parseIdentifierList :: TokenParser [JC.Identifier]
parseIdentifierList ts =
        parseIdentifier ts
    >>= \(name, ts)
    ->  parseIdentifierListOptions ts
    >>= \(names, ts)
    ->  return (name:names, ts)

parseVarDecs :: TokenParser [JC.VarDec]
parseVarDecs ts =
        case parseVarDec ts of
            Right (varDec, ts) -> case parseVarDecs ts of
                                    Right (varDecs, ts') -> Right (varDec:varDecs, ts')
                                    Left _             -> Right ([varDec], ts)
            Left _           -> Right ([], ts)

parseVarDec :: TokenParser JC.VarDec
parseVarDec ts =
        skipVarKw ts
    >>= \(_, ts)
    ->  parseJackType ts
    >>= \(jackType, ts)
    ->  parseIdentifierList ts
    >>= \(varNames, ts)
    ->  skipSemicolon ts
    >>= \(_, ts)
    ->  return (JC.VarDec jackType varNames, ts)

parseStatements :: TokenParser [JC.Statement]
parseStatements ts =
    case parseStatement ts of
        Right (stmt, ts) -> case parseStatements ts of
                                Right (stmts, ts') -> Right (stmt:stmts, ts')
                                Left _           -> Right ([stmt], ts)
        Left _ -> Right ([], ts)

parseStatement :: TokenParser JC.Statement
parseStatement ts =
        parseLetStatement ts
    <|> parseIfStatement ts
    <|> parseWhileStatement ts
    <|> parseDoStatement ts
    <|> parseReturnStatement ts

parseLetStatementName :: TokenParser JC.LetStatementName
parseLetStatementName ts =
        (   parseEArrayExp ts
        >>= \(arr, ts) -> Right (JC.LSA arr, ts))
    <|> (   parseIdentifier ts
        >>= \(varName, ts) -> Right (JC.LSV varName, ts) )

parseLetStatement :: TokenParser JC.Statement
parseLetStatement ts =
        skipLet ts
    >>= \(_, ts)
    ->  parseLetStatementName ts
    >>= \(name, ts)
    ->  skipEqual ts
    >>= \(_, ts)
    ->  parseExpression ts
    >>= \(expr, ts)
    ->  skipSemicolon ts
    >>= \(_, ts)
    ->  return (JC.LetStatement name expr, ts)


parseElseStatement :: TokenParser (Maybe [JC.Statement])
parseElseStatement ts =
        (   skipElse ts
        >>= \(_, ts)
        ->  skipLCurlyBracket ts
        >>= \(_, ts)
        ->  parseStatements ts
        >>= \(stmts, ts)
        ->  skipRCurlyBracket ts
        >>= \(_, ts)
        ->  return (Just stmts, ts))
    <|> return (Nothing, ts)

parseIfStatement :: TokenParser JC.Statement
parseIfStatement ts =
        skipIf ts
    >>= \(_, ts)
    ->  skipLParen ts
    >>= \(_, ts)
    ->  parseExpression ts
    >>= \(expr, ts)
    ->  skipRParen ts
    >>= \(_, ts)
    ->  skipLCurlyBracket ts
    >>= \(_, ts)
    ->  parseStatements ts
    >>= \(ifStmts, ts)
    ->  skipRCurlyBracket ts
    >>= \(_, ts)
    ->  parseElseStatement ts
    >>= \(elseStmts, ts)
    ->  return (JC.IfStatement expr ifStmts elseStmts, ts)
    

parseWhileStatement :: TokenParser JC.Statement
parseWhileStatement ts =
        skipWhile ts
    >>= \(_, ts)
    ->  skipLParen ts
    >>= \(_, ts)
    ->  parseExpression ts
    >>= \(expr, ts)
    ->  skipRParen ts
    >>= \(_, ts)
    ->  skipLCurlyBracket ts
    >>= \(_, ts)
    ->  parseStatements ts
    >>= \(stmts, ts)
    ->  skipRCurlyBracket ts
    >>= \(_, ts)
    ->  return (JC.WhileStatement expr stmts, ts)

parseDoStatement :: TokenParser JC.Statement
parseDoStatement ts =
        skipDo ts
    >>= \(_, ts)
    ->  parseSubroutineCall ts
    >>= \(subCall, ts)
    ->  skipSemicolon ts
    >>= \(_, ts)
    ->  return (JC.DoStatement subCall, ts)

parseReturnStatement :: TokenParser JC.Statement
parseReturnStatement ts =
        skipReturn ts
    >>= \(_, ts)
    ->  case parseExpression ts of
            Right (expr, ts') -> Right (JC.ReturnStatement (Just expr), ts')
            Left _          -> Right (JC.ReturnStatement Nothing, ts)
    >>= \(retStatement, ts)
    ->  skipSemicolon ts
    >>= \(_, ts)
    ->  return (retStatement, ts)

parseExpression :: TokenParser JC.Expression
parseExpression ts =
        parseTermOpTermExp ts
    <|> parseSingleTermExp ts

parseTermOpTermExp :: TokenParser JC.Expression
parseTermOpTermExp ts =
        parseTerm ts
    >>= \(term1, ts)
    ->  parseOp ts
    >>= \(op, ts)
    ->  parseTerm ts
    >>= \(term2, ts)
    ->  return (JC.ETermOpTerm term1 op term2, ts)

parseSingleTermExp :: TokenParser JC.Expression
parseSingleTermExp ts =
    case parseTerm ts of
        Right (res, ts) -> Right (JC.ESingleTerm res, ts)
        Left (err, errTkns)        -> Left (err <> " parseSingleTermExp ", errTkns)

instance Alternative (Either (String, [TK.Token])) where
    Right res <|> _ = Right res
    Left _    <|> Right res = Right res
    Left (err, errTkns)  <|> Left (err', _) = Left (err <> " + " <> err', errTkns)

parseTerm :: TokenParser JC.Term
parseTerm ts =
        parseIntegerConstant ts
    <|> parseStringConstant ts
    <|> parseKeywordConstantTerm ts
    <|> parseArrayExpTerm ts
    <|> parseSubroutineCallTerm ts
    <|> parseParenExpression ts
    <|> parseUnaryOpTerm ts
    <|> parseVarNameTerm ts

parseIntegerConstant :: TokenParser JC.Term
parseIntegerConstant [] = Left ("expected input in parseIntegerConstant", [])
parseIntegerConstant (t:ts) =
    case t of
        (TK.IC i) -> Right (JC.TIntegerConstant i, ts)
        _      -> Left ("failed parse in parseIntegerConstant", t:ts)

parseStringConstant :: TokenParser JC.Term
parseStringConstant [] = Left ("expected input in parseStringConstant", [])
parseStringConstant (t:ts) =
    case t of
        (TK.SC s) -> Right (JC.TStringConstant s, ts)
        _      -> Left ("failed parse in parseIntegerConstant", t:ts)

parseEKeywordConstant :: TokenParser JC.EKeywordConstant
parseEKeywordConstant [] = Left ("expected input in parseEKeywordConstant", [])
parseEKeywordConstant (t:ts) =
    case t of
        (TK.KW TK.TrueKW)     -> Right (JC.EKConTrue, ts)
        (TK.KW TK.FalseKW)    -> Right (JC.EKConFalse, ts)
        (TK.KW TK.Null)       -> Right (JC.EKConNull, ts)
        (TK.KW TK.This)       -> Right (JC.EKConThis, ts)
        _               -> Left ("failed parse in parseEKeywordConstant", t:ts)

parseKeywordConstantTerm :: TokenParser JC.Term
parseKeywordConstantTerm ts =
    case parseEKeywordConstant ts of
        Right (res, ts) -> Right (JC.TKeywordConstant res, ts)
        Left (err, errTkns)        -> Left (err <> " parseKeywordConstantTerm ", errTkns)

-- TODO delete
-- parseVarName :: TokenParser VarName
-- parseVarName [] = Left ("expected input in parseVarName", [])
-- parseVarName (t:ts) =
--     case t of
--         (ID name) -> Right (VarName name, ts)
--         _         -> Left ("failed parse in parseVarName", t:ts)

parseVarNameTerm :: TokenParser JC.Term
parseVarNameTerm ts = case parseIdentifier ts of
    Right (varName, ts) -> Right (JC.TVarName varName, ts)
    Left (err, errTkns)            -> Left (err <> " parseVarNameTerm ", errTkns)

parseEArrayExp :: TokenParser JC.ArrayExp --todo create state, maybe monad comb.
parseEArrayExp ts = 
        parseIdentifier ts
    >>= \(varName, ts)
    ->  skipLSquareBracket ts
    >>= \(_, ts)
    ->  parseExpression ts
    >>= \(expr, ts)
    ->  skipRSquareBracket ts
    >>= \(_, ts)
    ->  return (JC.ArrayExp varName expr, ts)

parseArrayExpTerm :: TokenParser JC.Term
parseArrayExpTerm ts =
    case parseEArrayExp ts of
        Right (res, ts) -> Right (JC.TArrayExp res, ts)
        Left (err, errTkns)        -> Left (err <> " parseArrayExpTerm ", errTkns)

        --TODO Delete
-- parseSubroutineName :: TokenParser SubroutineName
-- parseSubroutineName [] = Left ("expected input in parseSubroutineName", [])
-- parseSubroutineName (t:ts) =
--     case t of
--         (ID i) -> Right (SubroutineName i, ts)
--         _            -> Left ("failed parse in parseSubroutineName", t:ts)

-- parseClassName :: TokenParser ClassName
-- parseClassName [] = Left ("expected input in parseClassName", [])
-- parseClassName (t:ts) =
--     case t of
--         (ID i) -> Right (ClassName i, ts)
--         _            -> Left ("failed parse in parseClassName", t:ts)

parseIdentifier :: TokenParser JC.Identifier
parseIdentifier [] = Left ("expected input in parseIdentifier", [])
parseIdentifier (t:ts) =
    case t of
        (TK.ID i) -> Right (JC.Identifier i, ts)
        _            -> Left ("failed parse in parseIdentifier", t:ts)

parseSubroutineCallSimple :: TokenParser JC.SubroutineCall
parseSubroutineCallSimple ts =
        parseIdentifier ts
    >>= \(subName, ts)
    ->  skipLParen ts
    >>= \(_, ts)
    ->  parseExpressionList ts
    >>= \(exprs, ts)
    ->  skipRParen ts
    >>= \(_, ts)
    ->  return (JC.SR 
                JC.BasicSRCall { JC.srName = subName 
                               , JC.srExpressions = exprs}, ts)



parseSubroutineCallClass :: TokenParser JC.SubroutineCall
parseSubroutineCallClass ts =
        parseIdentifier ts
    >>= \(className, ts)
    ->  skipFullStop ts
    >>= \(_, ts)
    ->  parseIdentifier ts
    >>= \(subName, ts)
    ->  skipLParen ts
    >>= \(_, ts)
    ->  parseExpressionList ts
    >>= \(exprs, ts)
    ->  skipRParen ts
    >>= \(_, ts)
    ->  return (JC.SRCN { JC.srcnClName = className
                        , JC.srcnSR = JC.BasicSRCall { JC.srName = subName 
                                                     , JC.srExpressions = exprs}}, ts) 

parseSubroutineCallVar :: TokenParser JC.SubroutineCall
parseSubroutineCallVar ts =
        parseIdentifier ts
    >>= \(varName, ts)
    ->  skipFullStop ts
    >>= \(_, ts)
    ->  parseIdentifier ts
    >>= \(subName, ts)
    ->  skipLParen ts
    >>= \(_, ts)
    ->  parseExpressionList ts
    >>= \(exprs, ts)
    ->  skipRParen ts
    >>= \(_, ts)
    ->  return (JC.SRVN { JC.srvnVName = varName
                        , JC.srvnSR = JC.BasicSRCall { JC.srName = subName
                                                     , JC.srExpressions = exprs}}, ts) -- todo use parseSubroutineSimple

parseSubroutineCall :: TokenParser JC.SubroutineCall
parseSubroutineCall ts =
        case parseSubroutineCallClass ts of
            Right res -> Right res
            Left _ -> case parseSubroutineCallVar ts of
                Right res -> Right res
                Left _ -> case parseSubroutineCallSimple ts of
                    Right res -> Right res
                    Left _ -> Left ("failed parse in parseSubroutineCall", ts)

parseSubroutineCallTerm :: TokenParser JC.Term
parseSubroutineCallTerm ts =
    case parseSubroutineCall ts of
        Right (res, ts) -> Right (JC.TSubroutineCall res, ts)
        Left (err, errTkns)        -> Left (err <> " parseSubroutineCallTerm ", errTkns)


parseParenExpression :: TokenParser JC.Term
parseParenExpression ts =
        skipLParen ts
    >>= \(_, ts)
    ->  parseExpression ts
    >>= \(expr, ts)
    ->  skipRParen ts
    >>= \(_, ts)
    ->  return (JC.TParenExpression expr, ts)

parseExpressionOption :: TokenParser JC.Expression
parseExpressionOption [] = Left ("expected input in parseExpressionOption", [])
parseExpressionOption ts =
    case skipComma ts of
        Right (_, ts) -> parseExpression ts
        Left (err, errTkns)      -> Left (err <> " parseExpressionOption ", errTkns)

parseExpressionsOption :: TokenParser [JC.Expression]
parseExpressionsOption ts =
    case parseExpressionOption ts of
        Right (expr, ts) -> case parseExpressionsOption ts of
                            Right (rsf, ts') -> Right (expr:rsf, ts')
        Left _         -> Right ([], ts)

parseExpressionList :: TokenParser [JC.Expression]
parseExpressionList [] = Left ("expected input in parseExpressionList", [])
parseExpressionList ts =
        case parseExpression ts of Right (expr, ts) -> parseExpressionsOption ts
                                                            >>= \(exprs, ts)
                                                            ->  return (expr:exprs, ts)
                                   Left _ -> Right ([], ts)
    
    

parseOp :: TokenParser JC.Op
parseOp [] = Left ("expected input in parseOp", [])
parseOp (t:ts) =
    case t of
        (TK.SY TK.Plus)           -> Right (JC.OpPlus, ts)
        (TK.SY TK.Minus)          -> Right (JC.OpMinus, ts)
        (TK.SY TK.Asterix)        -> Right (JC.OpMultiply, ts)
        (TK.SY TK.Slash)          -> Right (JC.OpDivide, ts)
        (TK.SY TK.Ampersand)      -> Right (JC.OpAnd, ts)
        (TK.SY TK.VerticalBar)    -> Right (JC.OpOr, ts)
        (TK.SY TK.LAngleBracket)  -> Right (JC.OpLT, ts)
        (TK.SY TK.RAngleBracket)  -> Right (JC.OpGT, ts)
        (TK.SY TK.Equal)          -> Right (JC.OpEqual, ts)
        _                   -> Left ("failed parse in parseOp", ts)

parseUnaryOp :: TokenParser JC.UnaryOp
parseUnaryOp [] = Left ("expected input in parseUnaryOp", [])
parseUnaryOp (t:ts) =
    case t of
        (TK.SY TK.Minus) -> Right (JC.UOPArithNegation, ts)
        (TK.SY TK.Tilde) -> Right (JC.UOPBitNegation, ts)
        _          -> Left ("failed parse in parseUnaryOp", ts)

parseUnaryOpTerm :: TokenParser JC.Term
parseUnaryOpTerm ts =
        parseUnaryOp ts
    >>= \(uop, ts)
    ->  parseTerm ts
    >>= \(term, ts)
    ->  return (JC.TUnaryOp uop term, ts)

-- ======================= --
-- PARSERS TO SKIP SYMBOLS --
-- ======================= --

skipToken :: TK.Token -> TokenParser ()
skipToken _ [] = Left ("expected input in skipToken", [])
skipToken tkn (t:ts) =
    if tkn == t then Right ((), ts)
                else Left ("failed parse in skipToken. "
                            <> "expected: "
                            <> show tkn
                            <> "\n"
                            <> "got: "
                            <> show t
                            <> "\n", t:ts)

skipVoid :: TokenParser ()
skipVoid =
    skipToken (TK.KW TK.Void)

skipField :: TokenParser ()
skipField =
    skipToken (TK.KW TK.Field)

skipClass :: TokenParser ()
skipClass =
    skipToken (TK.KW TK.Class)

skipStatic :: TokenParser ()
skipStatic =
    skipToken (TK.KW TK.Static)

skipFunction :: TokenParser ()
skipFunction =
    skipToken (TK.KW TK.Function)

skipConstructor :: TokenParser ()
skipConstructor =
    skipToken (TK.KW TK.Constructor)

skipMethod :: TokenParser ()
skipMethod =
    skipToken (TK.KW TK.Method)

skipIntKw :: TokenParser ()
skipIntKw =
    skipToken (TK.KW TK.Int)

skipCharKw :: TokenParser ()
skipCharKw =
    skipToken (TK.KW TK.Char)

skipVarKw :: TokenParser ()
skipVarKw =
    skipToken (TK.KW TK.Var)

skipBoolKw :: TokenParser ()
skipBoolKw =
    skipToken (TK.KW TK.Boolean)

skipLet :: TokenParser ()
skipLet =
    skipToken (TK.KW TK.Let)

skipIf :: TokenParser ()
skipIf =
    skipToken (TK.KW TK.If)

skipElse :: TokenParser ()
skipElse =
    skipToken (TK.KW TK.Else)

skipWhile :: TokenParser ()
skipWhile =
    skipToken (TK.KW TK.While)

skipDo :: TokenParser ()
skipDo =
    skipToken (TK.KW TK.Do)

skipReturn :: TokenParser ()
skipReturn =
    skipToken (TK.KW TK.Return)

skipComma :: TokenParser ()
skipComma =
    skipToken (TK.SY TK.Comma)

skipFullStop :: TokenParser ()
skipFullStop =
    skipToken (TK.SY TK.FullStop)

skipSemicolon :: TokenParser ()
skipSemicolon =
    skipToken (TK.SY TK.Semicolon)

skipEqual :: TokenParser ()
skipEqual =
    skipToken (TK.SY TK.Equal)

skipLParen :: TokenParser ()
skipLParen =
    skipToken (TK.SY TK.LParen)

skipRParen :: TokenParser ()
skipRParen =
    skipToken (TK.SY TK.RParen)

skipLSquareBracket :: TokenParser ()
skipLSquareBracket =
    skipToken (TK.SY TK.LSquareBracket)

skipRSquareBracket :: TokenParser ()
skipRSquareBracket =
    skipToken (TK.SY TK.RSquareBracket)

skipLCurlyBracket :: TokenParser ()
skipLCurlyBracket =
    skipToken (TK.SY TK.LCurlyBracket)

skipRCurlyBracket :: TokenParser ()
skipRCurlyBracket =
    skipToken (TK.SY TK.RCurlyBracket)