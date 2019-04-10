{-# LANGUAGE OverloadedStrings #-}

module Data.Jack.Class.ConversionTo.VMCode where

import qualified Data.Jack.Class.Model as JC
import qualified Data.VM.Model as VM

import Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as BS (ByteString, pack)
import qualified Data.Map.Strict as MAP
import System.FilePath (addExtension, dropExtension)

convertDirectory :: [JC.File] -> [VM.File]
convertDirectory = map convertSingleFile

convertSingleFile :: JC.File -> VM.File
convertSingleFile JC.File { JC.jackClass = jc
                          , JC.path      = fp } =
    let vmProg = evalState (convert jc) (initConversionState (JC.className jc) ) in
    VM.File { VM.program = makeVMLines vmProg
            , VM.path = addExtension (dropExtension fp) "vm" }

makeVMLines :: [VM.Instruction] -> [VM.Line]
makeVMLines is =
    go 0 is
    where go _ [] = []
          go lNum (i:is') =
                VM.Line { VM.instruction = i
                        , VM.lineNumber = lNum } : go (lNum + 1) is'

class Convert a where
    convert :: a -> State ConversionState [VM.Instruction]
class DeclareVariables a where
    declare :: a -> State ConversionState ()

instance Convert JC.JackClass where
    convert JC.JackClass { JC.className = nm
                         , JC.classVarDecs = vds 
                         , JC.subroutineDecs = srds } = do
        let decCVDs [] = return ()
            decCVDs (d:ds) = do
                declare d
                decCVDs ds
            conSRDs [] = return []
            conSRDs (d:ds) = do
                d' <- convert d
                ds' <- conSRDs ds
                return (d'<>ds')
        decCVDs vds
        conSRDs srds

instance DeclareVariables JC.ClassVarDec where
    declare JC.ClassVarDec { JC.classVarKind = k
                           , JC.jackType = t
                           , JC.varNames = vns } =
        foldr (\ v -> (>>) (insertClassEntry v t k)) (return ()) vns

declareArgVar :: JC.VarDec -> State ConversionState ()
declareArgVar (JC.VarDec t vns) =
    foldr (\ v -> (>>) (insertMethodEntry v t ARG)) (return ()) vns
declareArgVars :: [JC.VarDec] -> State ConversionState ()
declareArgVars = foldr ((>>) . declareArgVar) (return ())

declareLocalVar :: JC.VarDec -> State ConversionState Int
declareLocalVar (JC.VarDec t vns) =
    foldr (\ v -> (>>) (insertMethodEntry v t VAR)) (return (length vns)) vns
declareLocalVars :: [JC.VarDec] -> State ConversionState Int
declareLocalVars [] = return 0
declareLocalVars (v:vs) = do
    n <- declareLocalVar v
    ns <- declareLocalVars vs
    return (n+ns)

declareParameters :: [JC.Parameter] -> State ConversionState ()
declareParameters = foldr ((>>) . declareParameter) (return ())
declareParameter :: JC.Parameter -> State ConversionState ()
declareParameter (JC.Param t i) =
    insertMethodEntry i t ARG

instance Convert JC.SubroutineDec where
    convert JC.SubroutineDec { JC.subroutineKind = k
                             , JC.subroutineType = t
                             , JC.subroutineName = nm
                             , JC.parameters = ps
                             , JC.subroutineBody =
                                    JC.SubroutineBody vDecs ss } = do
        resetMethodSymbolTable
        declareParameters ps
        numLocalVars <- declareLocalVars vDecs
        st <- get
        let fName = currentClassName st <> "." <> nm
            nVars = toInteger $ numLocalVars + (case k of
                                                JC.SRMethod -> 1
                                                _ -> 0)
        ss' <- convertStatements ss
        return $    [VM.F_VM $ VM.FUN fName nVars]
                <>   (case k of
                                JC.SRFunction -> []
                                _ -> undefined)
                <>  ss'   


convertStatements :: [JC.Statement] -> State ConversionState [VM.Instruction]
convertStatements [] = return []
convertStatements (s:ss) = do
    s' <- convert s
    ss' <- convertStatements ss
    return (s'<>ss')

convertMBStatements :: Maybe [JC.Statement] -> State ConversionState [VM.Instruction]
convertMBStatements Nothing = return []
convertMBStatements (Just ss) = convertStatements ss

instance Convert JC.Statement where
    convert (JC.LetStatement nm e) = do
        e' <- convert e
        popV <- case nm of
                    JC.LSV i -> popVar i
                    _        -> undefined
        return $ e' <> popV
    convert (JC.IfStatement c ss ess) = do
        cond <- convert c
        ifSs <- convertStatements ss
        elseSs <- convertMBStatements ess
        label1 <- takeNextLabel
        label2 <- takeNextLabel

        return $    cond
                <>  [   VM.AL_VM VM.NOT
                    ,   VM.P_VM $ VM.IF_GOTO label1 ]
                <>  ifSs
                <>  [   VM.P_VM $ VM.GOTO label2
                    ,   VM.P_VM $ VM.LABEL label1 ]
                <>  elseSs
                <>  [   VM.P_VM $ VM.LABEL label2 ]

    convert (JC.WhileStatement c ss) = do
        cond <- convert c
        wSs <- convertStatements ss
        label1 <- takeNextLabel
        label2 <- takeNextLabel
        return $    [   VM.P_VM $ VM.LABEL label1 ]
                <>  cond
                <>  [   VM.AL_VM VM.NOT
                    ,   VM.P_VM $ VM.IF_GOTO label2 ]
                <>  wSs
                <>  [   VM.P_VM $ VM.GOTO label1
                    ,   VM.P_VM $ VM.LABEL label2 ]



    convert (JC.DoStatement sc) = do
        sc' <- convert sc
        return $   sc'
                <> [    VM.M_VM $ VM.MemCMD VM.POP VM.TEMP 0 ]
    convert (JC.ReturnStatement me) = do
        e <- convertRetExpr me
        return $ e <> [VM.F_VM VM.RETURN]
        where convertRetExpr me' =
                case me' of
                    Just expr ->    convert expr
                    Nothing   ->    return [VM.M_VM $ VM.MemCMD VM.PUSH VM.CONSTANT 0]
            

instance Convert JC.Expression where
    convert (JC.ESingleTerm t) = convert t
    convert JC.ETermOpTerm { JC.lTerm = l
                           , JC.op    = o
                           , JC.rTerm = r } = do
        l' <- convert l
        r' <- convert r
        o' <- convert o
        return $  l' <> r' <> o'

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
    convert (JC.TKeywordConstant kc) = convert kc
    convert (JC.TVarName nm) = pushVar nm
    convert (JC.TArrayExp ae) = convert ae
    convert (JC.TSubroutineCall src) = convert src
    convert (JC.TParenExpression pe) = convert pe
    convert (JC.TUnaryOp uop t) = do
        t' <- convert t
        uop' <- convert uop
        return $ t' <> uop'

instance Convert JC.EKeywordConstant where
    convert JC.EKConTrue =
        return [VM.M_VM $ VM.MemCMD VM.PUSH VM.CONSTANT 1,
                VM.AL_VM VM.NEG]
    convert JC.EKConFalse =
        return [VM.M_VM $ VM.MemCMD VM.PUSH VM.CONSTANT 0 ]
    convert JC.EKConNull =
        return [VM.M_VM $ VM.MemCMD VM.PUSH VM.CONSTANT 0 ]
    convert JC.EKConThis =
        return [VM.M_VM $ VM.MemCMD VM.PUSH VM.THIS 0 ]

instance Convert JC.ArrayExp where
    convert JC.ArrayExp { JC.arrayExpName = nm 
                         , JC.arrayExp     = ae } =
        undefined

instance Convert JC.SubroutineCall where
    convert (JC.SR JC.BasicSRCall { JC.srName = snm
                                    , JC.srExpressions = es } ) = do
        st <- get
        let className = currentClassName st
            fName     = className <> "." <> snm
        es' <- convertExpressions es
        return $    es'
               <>   [   VM.F_VM $ VM.CALL fName (toInteger (length es)) ]
    convert JC.SRCN { JC.srcnClName = cnm
                    , JC.srcnSR     = 
                        bsrc@JC.BasicSRCall { JC.srName = snm
                                            , JC.srExpressions = es } } = do
        es' <- convertExpressions es
        let fName = cnm <> "." <> snm
        return $    es'
               <>   [   VM.F_VM $ VM.CALL fName (toInteger (length es)) ]
                 
    convert JC.SRVN { JC.srvnVName = nm
                    , JC.srvnSR    = 
                        bsrc@JC.BasicSRCall { JC.srName = snm
                                            , JC.srExpressions = es } } = do
        objType <- getObjectType nm
        let fName = objType <> "." <> snm
        pushObj <- pushVar nm
        es' <- convertExpressions es
        return $    pushObj
               <>   es'
               <>   [   VM.F_VM $ VM.CALL fName (toInteger (1 + length es)) ]

convertExpressions :: [JC.Expression] -> State ConversionState [VM.Instruction]
convertExpressions [] = return []
convertExpressions (e:es) = do
    e' <- convert e
    es' <- convertExpressions es
    return (e' <> es')

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
    put $ st { classSymTab = MAP.insert n clEntry oldSymTab 
             , nextStaticIndex = varNum + 1 }

insertClassEntry n t k@JC.CVField = do
    st <- get
    let varNum = nextFieldIndex st
        oldSymTab = classSymTab st
        clEntry = ClassEntry { clName = n
                             , clType = t
                             , clKind = k
                             , clIndex = varNum }
    put $ st { classSymTab = MAP.insert n clEntry oldSymTab
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
    put $ st { metSymTab = MAP.insert n mEntry oldSymTab
             , nextArgIndex = varNum + 1 }

insertMethodEntry n t k@VAR = do
    st <- get
    let varNum = nextVarIndex st
        oldSymTab = metSymTab st
        mEntry = MethodEntry { mName = n
                             , mType = t
                             , mKind = k
                             , mIndex = varNum }
    put $ st { metSymTab = MAP.insert n mEntry oldSymTab
             , nextVarIndex = varNum + 1 }

takeNextLabel :: State ConversionState BS.ByteString
takeNextLabel = do
    st <- get
    let lbl = nextLabelVal st
    put $ st { nextLabelVal = lbl + 1 }
    return $ BS.pack ("LABEL_$" <> show lbl)

resetMethodSymbolTable :: State ConversionState ()
resetMethodSymbolTable = do
    st <- get
    put $ st { metSymTab = initMethodSymbolTable
             , nextVarIndex = 0
             , nextArgIndex = 0 }

lookupMetSymTab :: JC.Identifier -> State ConversionState (Maybe MethodEntry)
lookupMetSymTab i = MAP.lookup i . metSymTab <$> get
lookupClassSymTab :: JC.Identifier -> State ConversionState (Maybe ClassEntry)
lookupClassSymTab i = MAP.lookup i . classSymTab <$> get

getObjectType :: JC.Identifier -> State ConversionState BS.ByteString
getObjectType nm = do
        res <- lookupMetSymTab nm
        case res of
                Just me -> case mType me of
                                JC.ClassType oType -> return oType
                                _ -> error "can't call method on primitive types"
                Nothing -> do
                    res' <- lookupClassSymTab nm
                    case res' of
                        Just me -> case clType me of
                                    JC.ClassType oType -> return oType
                                    _ -> error "can't call method on primitive types"
                        Nothing -> error "undefined object"

data ConversionState = ConversionState { classSymTab :: ClassSymbolTable
                                       , metSymTab   :: MethodSymbolTable
                                       , nextStaticIndex :: Integer
                                       , nextFieldIndex :: Integer
                                       , nextArgIndex :: Integer
                                       , nextVarIndex :: Integer
                                       , nextLabelVal :: Integer
                                       , currentClassName    :: BS.ByteString
                                       , methodName   :: Maybe BS.ByteString }
initConversionState :: JC.Identifier -> ConversionState
initConversionState cn = 
    ConversionState { classSymTab = initClassSymbolTable
                    , metSymTab   = initMethodSymbolTable
                    , nextStaticIndex = 0
                    , nextFieldIndex  = 0
                    , nextArgIndex    = 0
                    , nextVarIndex    = 0
                    , nextLabelVal    = 0
                    , currentClassName       = cn
                    , methodName = Nothing }

pushVar :: JC.Identifier -> State ConversionState [VM.Instruction]
pushVar i = do
    res <- lookupMetSymTab i
    case res of
        Just mEntry -> pushMethodEntry mEntry
        Nothing -> do
            res' <- lookupClassSymTab i
            case res' of
                Just clEntry -> pushClassEntry clEntry
                Nothing      -> error "undefined variable" -- todo add in maybe datatype to conversion


popVar :: JC.Identifier -> State ConversionState [VM.Instruction]
popVar i = do
    res <- lookupMetSymTab i
    case res of
        Just mEntry -> popMethodEntry mEntry
        Nothing -> do
            res' <- lookupClassSymTab i
            case res' of
                Just clEntry -> popClassEntry clEntry
                Nothing      -> error "undefined variable" -- todo add in maybe datatype to conversion


pushClassEntry :: ClassEntry -> State ConversionState [VM.Instruction]
pushClassEntry ClassEntry { clKind = k 
                          , clIndex = i } =
        return  [   VM.M_VM $
                        VM.MemCMD
                        VM.PUSH 
                        (case k of
                            JC.CVStatic -> VM.STATIC
                            JC.CVField  -> VM.THIS)
                        i ]

popClassEntry :: ClassEntry -> State ConversionState [VM.Instruction]
popClassEntry ClassEntry {  clKind = k 
                         , clIndex = i } =
        return  [   VM.M_VM $
                        VM.MemCMD
                        VM.POP 
                        (case k of
                            JC.CVStatic -> VM.STATIC
                            JC.CVField  -> VM.THIS)
                        i ]

pushMethodEntry :: MethodEntry -> State ConversionState [VM.Instruction]
pushMethodEntry MethodEntry { mKind = k 
                            , mIndex = i } =
        return  [   VM.M_VM $
                        VM.MemCMD
                        VM.PUSH 
                        (case k of
                            ARG -> VM.ARGUMENT
                            VAR -> VM.LOCAL)
                        i ]

popMethodEntry :: MethodEntry -> State ConversionState [VM.Instruction]
popMethodEntry MethodEntry {  mKind = k 
                            , mIndex = i } =
        return  [   VM.M_VM $
                        VM.MemCMD
                        VM.POP 
                        (case k of
                            ARG -> VM.ARGUMENT
                            VAR -> VM.LOCAL)
                        i ]

type ClassSymbolTable = MAP.Map JC.Identifier ClassEntry
data ClassEntry = ClassEntry { clName :: JC.Identifier
                                , clType :: JC.JackType
                                , clKind :: JC.ClassVarKind
                                , clIndex :: Integer }
initClassSymbolTable :: ClassSymbolTable
initClassSymbolTable = MAP.empty

type MethodSymbolTable = MAP.Map JC.Identifier MethodEntry
data MethodEntry = MethodEntry { mName :: JC.Identifier
                               , mType :: JC.JackType
                               , mKind :: MethodKind
                               , mIndex :: Integer }
data MethodKind =
    ARG
  | VAR
initMethodSymbolTable :: MethodSymbolTable
initMethodSymbolTable = MAP.empty
