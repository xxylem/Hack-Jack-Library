{-# LANGUAGE OverloadedStrings #-}

module Data.Jack.Class.ConversionTo.VMCode where

import qualified Data.Jack.Class.Model as JC
import qualified Data.VM.Model as VM

import Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as BS (ByteString)
import qualified Data.Map.Strict as Map

insertClassEntry :: JC.Identifier 
                 -> JC.JackType
                 -> JC.ClassVarKind 
                 -> State ConversionState ()
insertClassEntry n t k@JC.CVStatic = do
    st <- get
    let varNum = nextStaticIndex st
        oldSymTab = classSymTab st
        clEntry = ClassEntry { clName = n
                             , clType = t
                             , clKind = k
                             , clIndex = varNum }
    put $ st { classSymTab = Map.insert n clEntry oldSymTab 
             , nextStaticIndex = varNum + 1 }

insertClassEntry n t k@JC.CVField = do
    st <- get
    let varNum = nextFieldIndex st
        oldSymTab = classSymTab st
        clEntry = ClassEntry { clName = n
                             , clType = t
                             , clKind = k
                             , clIndex = varNum }
    put $ st { classSymTab = Map.insert n clEntry oldSymTab
             , nextFieldIndex = varNum + 1 }

insertMethodEntry :: JC.Identifier 
                    -> JC.JackType
                    -> MethodKind
                    -> State ConversionState ()
insertMethodEntry n t k@ARG = do
    st <- get
    let varNum = nextArgIndex st
        oldSymTab = metSymTab st
        mEntry = MethodEntry { mName = n
                             , mType = t
                             , mKind = k
                             , mIndex = varNum }
    put $ st { metSymTab = Map.insert n mEntry oldSymTab
             , nextArgIndex = varNum + 1 }

insertMethodEntry n t k@VAR = do
    st <- get
    let varNum = nextVarIndex st
        oldSymTab = metSymTab st
        mEntry = MethodEntry { mName = n
                             , mType = t
                             , mKind = k
                             , mIndex = varNum }
    put $ st { metSymTab = Map.insert n mEntry oldSymTab
             , nextVarIndex = varNum + 1 }

data ConversionState = ConversionState { classSymTab :: ClassSymbolTable
                                       , metSymTab   :: MethodSymbolTable
                                       , nextStaticIndex :: Integer
                                       , nextFieldIndex :: Integer
                                       , nextArgIndex :: Integer
                                       , nextVarIndex :: Integer }
initConversionState :: ConversionState
initConversionState = 
    ConversionState { classSymTab = initClassSymbolTable
                    , metSymTab   = initMethodSymbolTable
                    , nextStaticIndex = 0
                    , nextFieldIndex  = 0
                    , nextArgIndex    = 0
                    , nextVarIndex    = 0 }

type ClassSymbolTable = Map.Map JC.Identifier ClassEntry
data ClassEntry = ClassEntry { clName :: JC.Identifier
                                , clType :: JC.JackType
                                , clKind :: JC.ClassVarKind
                                , clIndex :: Integer }
initClassSymbolTable :: ClassSymbolTable
initClassSymbolTable = Map.empty

type MethodSymbolTable = Map.Map JC.Identifier MethodEntry
data MethodEntry = MethodEntry { mName :: JC.Identifier
                               , mType :: JC.JackType
                               , mKind :: MethodKind
                               , mIndex :: Integer }
data MethodKind =
    ARG
  | VAR
initMethodSymbolTable :: MethodSymbolTable
initMethodSymbolTable = Map.empty


convertDirectory :: [JC.File] -> [VM.File]
convertDirectory = undefined

convertSingleFile :: JC.File -> VM.File
convertSingleFile = undefined


class Convert a where
    convert :: a -> State ConversionState [VM.Instruction]

instance Convert JC.JackClass where
    convert JC.JackClass { JC.className = nm
                         , JC.classVarDecs = vds 
                         , JC.subroutineDecs = srds } =
        undefined

instance Convert JC.ClassVarDec where
    convert JC.ClassVarDec { JC.classVarKind = k
                           , JC.jackType = t
                           , JC.varNames = vns } =
        undefined

instance Convert JC.SubroutineDec where
    convert JC.SubroutineDec { JC.subroutineKind = k
                             , JC.subroutineType = t
                             , JC.subroutineName = nm
                             , JC.parameters = ps
                             , JC.subroutineBody = b } =
        undefined

instance Convert JC.Statement where
    convert (JC.LetStatement nm e) = undefined
    convert (JC.IfStatement c ss ess) = undefined
    convert (JC.WhileStatement c ss) = undefined
    convert (JC.DoStatement sc) = undefined
    convert (JC.ReturnStatement me) = undefined

instance Convert JC.Expression where
    convert (JC.ESingleTerm t) = undefined
    convert JC.ETermOpTerm { JC.lTerm = l
                           , JC.op    = o
                           , JC.rTerm = r } =
        undefined

instance Convert JC.Term where
    convert (JC.TIntegerConstant n) =
        return [VM.M_VM $ VM.MemCMD VM.PUSH VM.CONSTANT n]
    convert (JC.TStringConstant s) = undefined
        -- let s' = show s
        --     appendChars [] = []
        --     appendChars (c:cs) = undefined -- TODO how do we build string constant?
        -- in
        -- return [ VM.M_VM $ VM.MemCMD VM.PUSH VM.CONSTANT (length s')
        --        , VM.F_VM $ VM.CALL { VM.label = "String.new"
        --                            , VM.nArgs = 1 } ]
        --        <> appendChars s'
    convert (JC.TKeywordConstant kc) = undefined
    convert (JC.TVarName nm) = undefined
    convert (JC.TArrayExp ae) = undefined
    convert (JC.TSubroutineCall src) = undefined
    convert (JC.TParenExpression pe) = undefined
    convert (JC.TUnaryOp uop t) = undefined

instance Convert JC.EKeywordConstant where
    convert JC.EKConTrue =
        return [VM.M_VM $ VM.MemCMD VM.PUSH VM.CONSTANT 1,
                VM.AL_VM VM.NEG]
    convert JC.EKConFalse =
        return [VM.M_VM $ VM.MemCMD VM.PUSH VM.CONSTANT 0]
    convert JC.EKConNull =
        return [VM.M_VM $ VM.MemCMD VM.PUSH VM.CONSTANT 0]

instance Convert JC.ArrayExp where
    convert JC.ArrayExp { JC.arrayExpName = nm 
                         , JC.arrayExp     = ae } =
        undefined

instance Convert JC.SubroutineCall where
    convert (JC.SR bsrc) = undefined
    convert JC.SRCN { JC.srcnClName = nm
                    , JC.srcnSR     = bsrc } =
        undefined
    convert JC.SRVN { JC.srvnVName = nm
                    , JC.srvnSR    = bsrc  } = 
        undefined

instance Convert JC.BasicSRCall where
    convert JC.BasicSRCall { JC.srName = nm
                        , JC.srExpressions = es } = 
        undefined

instance Convert JC.Op where
    convert JC.OpPlus =
        return [VM.AL_VM VM.ADD]
    convert JC.OpMinus =
        return [VM.AL_VM VM.SUB]
    convert JC.OpMultiply =
        return [VM.F_VM VM.CALL { VM.label = "Math.multiply"
                                , VM.nArgs = 2 } ]
    convert JC.OpDivide =
        return [VM.F_VM VM.CALL { VM.label = "Math.divide"
                                , VM.nArgs = 2 } ]
    convert JC.OpAnd =
        return [VM.AL_VM VM.AND]
    convert JC.OpOr =
        return [VM.AL_VM VM.OR]
    convert JC.OpLT =
        return [VM.AL_VM VM.LT_VM]
    convert JC.OpGT =
        return [VM.AL_VM VM.GT_VM]
    convert JC.OpEqual =
        return [VM.AL_VM VM.EQ_VM]
    
    

instance Convert JC.UnaryOp where
    convert JC.UOPArithNegation =
        return [VM.AL_VM VM.NEG]
    convert JC.UOPBitNegation =
        return [VM.AL_VM VM.NOT]