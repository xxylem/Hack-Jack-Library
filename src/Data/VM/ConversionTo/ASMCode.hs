{-# LANGUAGE OverloadedStrings #-}

module Data.VM.ConversionTo.ASMCode where

import qualified Data.VM.Model as VM
import qualified Data.Hack.ASM.Model as ASM

import Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as BS (pack)
import System.FilePath (takeBaseName, dropTrailingPathSeparator, (</>))

data InFunctionState =
    Inside VM.Label
  | Outside

data ConversionState = 
    ConversionState { nextEq            :: Integer
                    , nextGt            :: Integer
                    , nextLt            :: Integer
                    , nextReturn        :: Integer
                    , inFunctionState   :: InFunctionState
                    , fileNameNoExt     :: FilePath }

initState :: FilePath -> ConversionState
initState fp = ConversionState { nextEq = 0
                            , nextGt = 0
                            , nextLt = 0
                            , nextReturn = 0
                            , inFunctionState = Outside
                            , fileNameNoExt = takeBaseName fp }

initCode :: State ConversionState [ASM.Instruction]
initCode = do
    sysInit <- convertCMD (VM.CALL { VM.label = "Sys.init"
                                   , VM.nArgs = 0 } )
    return $ [  ASM.A 256
             ,  dEqualsNJ ASM.A_COMP
             ,  ASM.S "SP"
             ,  mEqualsNJ ASM.D_COMP ]
             <> sysInit

convertDirectory :: FilePath -> [VM.File] -> ASM.File
convertDirectory dirName fs =
    let fileName = dirName </> takeBaseName (dropTrailingPathSeparator dirName)
        (initInstrs, st) = runState initCode (initState dirName)
        restInstrs = evalState (convertFilesToInstructions fs) st
    in ASM.makeFileFromInstructions fileName (initInstrs <> restInstrs)

convertSingleFile :: VM.File -> ASM.File
convertSingleFile vmFile@VM.File { VM.path    = vmPath } =
    -- let (initInstrs, st) = runState initCode (initState vmPath)
       let instrs = evalState (convertFileToInstructions vmFile) (initState vmPath)
    in ASM.makeFileFromInstructions vmPath instrs
        

convertFilesToInstructions :: [VM.File] -> State ConversionState [ASM.Instruction]
convertFilesToInstructions [] = return []
convertFilesToInstructions (f:fs) = do
    f' <- convertFileToInstructions f
    fs' <- convertFilesToInstructions fs
    return $ f' <> fs'

convertFileToInstructions :: VM.File -> State ConversionState [ASM.Instruction]
convertFileToInstructions VM.File { VM.program = vmProg
                              , VM.path    = vmPath } = do 
    st <- get
    put $ st { fileNameNoExt = takeBaseName vmPath }
    convertVMLines vmProg

convertVMLines :: [VM.Line] -> State ConversionState [ASM.Instruction]
convertVMLines [] = return []
convertVMLines (l:ls) = do
    asmL <- convertVMLine l
    asmLs <- convertVMLines ls
    return (asmL <> asmLs)

convertVMLine :: VM.Line -> State ConversionState [ASM.Instruction]
convertVMLine VM.Line { VM.instruction = instr } = convertCMD instr


class ConvertCommand c where
    convertCMD :: c -> State ConversionState [ASM.Instruction]

instance ConvertCommand VM.Instruction where
    convertCMD (VM.AL_VM cmd) = convertCMD cmd
    convertCMD (VM.M_VM  cmd) = convertCMD cmd
    convertCMD (VM.P_VM  cmd) = convertCMD cmd
    convertCMD (VM.F_VM  cmd) = convertCMD cmd

aEqualsNJ :: ASM.Computation -> ASM.Instruction
aEqualsNJ c = ASM.C c ASM.A_DEST ASM.NULL_JUMP
mEqualsNJ :: ASM.Computation -> ASM.Instruction
mEqualsNJ c = ASM.C c ASM.M_DEST ASM.NULL_JUMP
dEqualsNJ :: ASM.Computation -> ASM.Instruction
dEqualsNJ c = ASM.C c ASM.D_DEST ASM.NULL_JUMP

instance ConvertCommand VM.MemoryAccessCommand where
    convertCMD VM.MemCMD { VM.direction = VM.PUSH
                            , VM.segment   = VM.ARGUMENT
                            , VM.index     = vmIndex } =
        return  [   ASM.A vmIndex
                ,   dEqualsNJ ASM.A_COMP
                ,   ASM.S "ARG"
                ,   aEqualsNJ ASM.D_PLUS_M
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_COMP
                ,   mEqualsNJ ASM.D_COMP
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.M_PLUS_1 ]

    convertCMD VM.MemCMD { VM.direction = VM.POP
                            , VM.segment   = VM.ARGUMENT
                            , VM.index     = vmIndex } =
        return  [   ASM.A vmIndex
                ,   dEqualsNJ ASM.A_COMP
                ,   ASM.S "ARG"
                ,   dEqualsNJ ASM.D_PLUS_M
                ,   ASM.S "R13"
                ,   mEqualsNJ ASM.D_COMP
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.M_MINUS_1
                ,   aEqualsNJ ASM.M_COMP
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "R13"
                ,   aEqualsNJ ASM.M_COMP
                ,   mEqualsNJ ASM.D_COMP ]

    convertCMD VM.MemCMD { VM.direction = VM.PUSH
                            , VM.segment   = VM.LOCAL
                            , VM.index     = vmIndex } =
        return  [   ASM.A vmIndex
                ,   dEqualsNJ ASM.A_COMP
                ,   ASM.S "LCL"
                ,   aEqualsNJ ASM.D_PLUS_M
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_COMP
                ,   mEqualsNJ ASM.D_COMP
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.M_PLUS_1 ]
     
    convertCMD VM.MemCMD { VM.direction = VM.POP
                            , VM.segment   = VM.LOCAL
                            , VM.index     = vmIndex } =
        return  [   ASM.A vmIndex
                ,   dEqualsNJ ASM.A_COMP
                ,   ASM.S "LCL"
                ,   dEqualsNJ ASM.D_PLUS_M
                ,   ASM.S "R13"
                ,   mEqualsNJ ASM.D_COMP
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.M_MINUS_1
                ,   aEqualsNJ ASM.M_COMP
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "R13"
                ,   aEqualsNJ ASM.M_COMP
                ,   mEqualsNJ ASM.D_COMP ]
  
    convertCMD VM.MemCMD { VM.direction = VM.PUSH
                            , VM.segment   = VM.STATIC
                            , VM.index     = vmIndex } = do
        st <- get
        let varName = BS.pack (fileNameNoExt st) <> "." <> BS.pack (show vmIndex)
        return  [   ASM.S varName
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_COMP
                ,   mEqualsNJ ASM.D_COMP
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.M_PLUS_1 ]

    convertCMD VM.MemCMD { VM.direction = VM.POP
                            , VM.segment   = VM.STATIC
                            , VM.index     = vmIndex } = do
        st <- get
        let varName = BS.pack (fileNameNoExt st) <> "." <> BS.pack (show vmIndex)
        return  [   ASM.S "SP"
                ,   mEqualsNJ ASM.M_MINUS_1
                ,   aEqualsNJ ASM.M_COMP
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S varName
                ,   mEqualsNJ ASM.D_COMP ]

    convertCMD VM.MemCMD { VM.direction = VM.PUSH
                            , VM.segment   = VM.CONSTANT
                            , VM.index     = vmIndex } =
        return  [   ASM.A vmIndex
                ,   dEqualsNJ ASM.A_COMP
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_COMP
                ,   mEqualsNJ ASM.D_COMP
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.M_PLUS_1 ]

    convertCMD VM.MemCMD { VM.direction = VM.POP
                         , VM.segment   = VM.CONSTANT } =
        error "impossible program state reached"

    convertCMD VM.MemCMD { VM.direction = VM.PUSH
                            , VM.segment   = VM.THIS
                            , VM.index     = vmIndex } =
        return  [   ASM.A vmIndex
                ,   dEqualsNJ ASM.A_COMP
                ,   ASM.S "THIS"
                ,   aEqualsNJ ASM.D_PLUS_M
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_COMP
                ,   mEqualsNJ ASM.D_COMP
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.M_PLUS_1 ]

    convertCMD VM.MemCMD { VM.direction = VM.POP
                            , VM.segment   = VM.THIS
                            , VM.index     = vmIndex } =
        return  [   ASM.A vmIndex
                ,   dEqualsNJ ASM.A_COMP
                ,   ASM.S "THIS"
                ,   dEqualsNJ ASM.D_PLUS_M
                ,   ASM.S "R13"
                ,   mEqualsNJ ASM.D_COMP
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.M_MINUS_1
                ,   aEqualsNJ ASM.M_COMP
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "R13"
                ,   aEqualsNJ ASM.M_COMP
                ,   mEqualsNJ ASM.D_COMP ]

    convertCMD VM.MemCMD { VM.direction = VM.PUSH
                            , VM.segment   = VM.THAT
                            , VM.index     = vmIndex } =
        return  [   ASM.A vmIndex
                ,   dEqualsNJ ASM.A_COMP
                ,   ASM.S "THAT"
                ,   aEqualsNJ ASM.D_PLUS_M
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_COMP
                ,   mEqualsNJ ASM.D_COMP
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.M_PLUS_1 ]

    convertCMD VM.MemCMD { VM.direction = VM.POP
                            , VM.segment   = VM.THAT
                            , VM.index     = vmIndex } =
        return  [   ASM.A vmIndex
                ,   dEqualsNJ ASM.A_COMP
                ,   ASM.S "THAT"
                ,   dEqualsNJ ASM.D_PLUS_M
                ,   ASM.S "R13"
                ,   mEqualsNJ ASM.D_COMP
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.M_MINUS_1
                ,   aEqualsNJ ASM.M_COMP
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "R13"
                ,   aEqualsNJ ASM.M_COMP
                ,   mEqualsNJ ASM.D_COMP ]
      
    convertCMD VM.MemCMD { VM.direction = VM.PUSH
                            , VM.segment   = VM.POINTER
                            , VM.index     = vmIndex } =
        return  [   ASM.A (vmIndex + 3)
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_COMP
                ,   mEqualsNJ ASM.D_COMP
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.M_PLUS_1 ]

    convertCMD VM.MemCMD { VM.direction = VM.POP
                            , VM.segment   = VM.POINTER
                            , VM.index     = vmIndex } =
        return  [   ASM.S "SP"
                ,   mEqualsNJ ASM.M_MINUS_1
                ,   aEqualsNJ ASM.M_COMP
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.A (vmIndex + 3)
                ,   mEqualsNJ ASM.D_COMP ]

    convertCMD VM.MemCMD { VM.direction = VM.PUSH
                            , VM.segment   = VM.TEMP
                            , VM.index     = vmIndex } =
        return  [   ASM.A (vmIndex + 5)
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_COMP
                ,   mEqualsNJ ASM.D_COMP
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.M_PLUS_1 ]

    convertCMD VM.MemCMD { VM.direction = VM.POP
                            , VM.segment   = VM.TEMP
                            , VM.index     = vmIndex } =
        return  [   ASM.S "SP"
                ,   mEqualsNJ ASM.M_MINUS_1
                ,   aEqualsNJ ASM.M_COMP
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.A (vmIndex + 5)
                ,   mEqualsNJ ASM.D_COMP ]

instance ConvertCommand VM.ArithLogicCommand where
    convertCMD VM.ADD =
        return  [   ASM.S "SP"
                ,   mEqualsNJ ASM.M_MINUS_1
                ,   aEqualsNJ ASM.M_COMP
                ,   dEqualsNJ ASM.M_COMP
                ,   aEqualsNJ ASM.A_MINUS_1
                ,   mEqualsNJ ASM.D_PLUS_M ] -- TODO check

    convertCMD VM.SUB =
        return  [   ASM.S "SP"
                ,   mEqualsNJ ASM.M_MINUS_1
                ,   aEqualsNJ ASM.M_COMP
                ,   dEqualsNJ ASM.M_COMP
                ,   aEqualsNJ ASM.A_MINUS_1
                ,   mEqualsNJ ASM.M_MINUS_D ]

    convertCMD VM.NEG =
        return  [   ASM.S "SP"
                ,   aEqualsNJ ASM.M_MINUS_1
                ,   mEqualsNJ ASM.NEG_M ]

    convertCMD VM.EQ_VM = do
        st <- get
        let currentEqVal = nextEq st
            currentEqLabel = BS.pack $ show currentEqVal
            trueJmp      = "TRUE_EQ_$" <> currentEqLabel
            endJmp       = "END_EQ_$"  <> currentEqLabel
        put (st { nextEq = currentEqVal + 1 })
        return  [   ASM.S "SP"
                ,   mEqualsNJ ASM.M_MINUS_1
                ,   aEqualsNJ ASM.M_COMP
                ,   dEqualsNJ ASM.M_COMP
                ,   aEqualsNJ ASM.A_MINUS_1
                ,   dEqualsNJ ASM.D_MINUS_M
                ,   ASM.S trueJmp
                ,   ASM.C ASM.D_COMP ASM.NULL_DEST ASM.JEQ
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_MINUS_1
                ,   mEqualsNJ ASM.ZERO
                ,   ASM.S endJmp 
                ,   ASM.C ASM.ZERO ASM.NULL_DEST ASM.JMP
                ,   ASM.L trueJmp
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_MINUS_1
                ,   mEqualsNJ ASM.MINUS_ONE
                ,   ASM.L endJmp ]

    convertCMD VM.GT_VM = do
        st <- get
        let currentGtVal = nextGt st
            currentGtLabel = BS.pack $ show currentGtVal
            trueJmp      = "TRUE_GT_$" <> currentGtLabel
            endJmp       = "END_GT_$"  <> currentGtLabel
        put (st { nextGt = currentGtVal + 1 })
        return  [   ASM.S "SP"
                ,   mEqualsNJ ASM.M_MINUS_1
                ,   aEqualsNJ ASM.M_COMP
                ,   dEqualsNJ ASM.M_COMP
                ,   aEqualsNJ ASM.A_MINUS_1
                ,   dEqualsNJ ASM.D_MINUS_M
                ,   ASM.S trueJmp
                ,   ASM.C ASM.D_COMP ASM.NULL_DEST ASM.JLT
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_MINUS_1
                ,   mEqualsNJ ASM.ZERO
                ,   ASM.S endJmp
                ,   ASM.C ASM.ZERO ASM.NULL_DEST ASM.JMP
                ,   ASM.L trueJmp
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_MINUS_1
                ,   mEqualsNJ ASM.MINUS_ONE
                ,   ASM.L endJmp ]

    convertCMD VM.LT_VM = do
        st <- get
        let currentLtVal = nextLt st
            currentLtLabel = BS.pack $ show currentLtVal
            trueJmp      = "TRUE_LT_$" <> currentLtLabel
            endJmp       = "END_LT_$"  <> currentLtLabel
        put (st { nextLt = currentLtVal + 1 })
        return  [   ASM.S "SP"
                ,   mEqualsNJ ASM.M_MINUS_1
                ,   aEqualsNJ ASM.M_COMP
                ,   dEqualsNJ ASM.M_COMP
                ,   aEqualsNJ ASM.A_MINUS_1
                ,   dEqualsNJ ASM.D_MINUS_M
                ,   ASM.S trueJmp
                ,   ASM.C ASM.D_COMP ASM.NULL_DEST ASM.JGT
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_MINUS_1
                ,   mEqualsNJ ASM.ZERO
                ,   ASM.S endJmp
                ,   ASM.C ASM.ZERO ASM.NULL_DEST ASM.JMP
                ,   ASM.L trueJmp
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_MINUS_1
                ,   mEqualsNJ ASM.MINUS_ONE
                ,   ASM.L endJmp ]

    convertCMD VM.AND =
        return  [   ASM.S "SP"
                ,   mEqualsNJ ASM.M_MINUS_1
                ,   aEqualsNJ ASM.M_COMP
                ,   dEqualsNJ ASM.M_COMP
                ,   aEqualsNJ ASM.A_MINUS_1
                ,   ASM.C ASM.D_AND_M ASM.M_DEST ASM.NULL_JUMP ]

    convertCMD VM.OR =
        return  [   ASM.S "SP"
                ,   mEqualsNJ ASM.M_MINUS_1
                ,   aEqualsNJ ASM.M_COMP
                ,   dEqualsNJ ASM.M_COMP
                ,   aEqualsNJ ASM.A_MINUS_1
                ,   mEqualsNJ ASM.D_OR_M ]

    convertCMD VM.NOT =
        return  [   ASM.S "SP"
                ,   aEqualsNJ ASM.M_MINUS_1
                ,   mEqualsNJ ASM.NOT_M ]

instance ConvertCommand VM.ProgramFlowCommand where
    convertCMD (VM.LABEL lb) = do
        st <- get
        let funSt = inFunctionState st
            label = case funSt of
                        Inside funName -> funName <> "$" <> lb
                        Outside        -> lb
        return  [   ASM.L label ]

    convertCMD (VM.GOTO lb) = do
        st <- get
        let funSt = inFunctionState st
            label = case funSt of
                        Inside funName -> funName <> "$" <> lb
                        Outside        -> lb
        return  [   ASM.S label 
                ,   ASM.C ASM.ZERO ASM.NULL_DEST ASM.JMP ]

    convertCMD (VM.IF_GOTO lb) = do
        st <- get
        let funSt = inFunctionState st
            label = case funSt of
                        Inside funName -> funName <> "$" <> lb
                        Outside        -> lb
        return  [   ASM.S "SP"
                ,   ASM.C ASM.M_MINUS_1 ASM.AM ASM.NULL_JUMP
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S label
                ,   ASM.C ASM.D_COMP ASM.NULL_DEST ASM.JNE ]


instance ConvertCommand VM.FunctionCommand where
    convertCMD VM.FUN { VM.label = label
                      , VM.nVars = nVars } = do
        st <- get
        put $ st { inFunctionState = Inside label }
        let nPush0 0 = []
            nPush0 n =  [   ASM.S "0"
                        ,   dEqualsNJ ASM.A_COMP
                        ,   ASM.S "SP"
                        ,   aEqualsNJ ASM.M_COMP
                        ,   mEqualsNJ ASM.D_COMP
                        ,   ASM.S "SP"
                        ,   mEqualsNJ ASM.M_PLUS_1 ]
                        <>  nPush0 (n-1)
        return  $ ASM.L label : nPush0 nVars

    convertCMD VM.CALL { VM.label = label 
                       , VM.nArgs = nArgs } = do
        st <- get
        let retVal = nextReturn st
            retAddLabel = "RETURN_LOCATION_$" <> BS.pack (show retVal)
        put ( st { nextReturn = retVal + 1 })
        return  [   --push return-address
                    ASM.S retAddLabel
                ,   dEqualsNJ ASM.A_COMP
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_COMP
                ,   mEqualsNJ ASM.D_COMP
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.M_PLUS_1

                    --push LCL
                ,   ASM.S "LCL"
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_COMP
                ,   mEqualsNJ ASM.D_COMP
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.M_PLUS_1

                    --push ARG
                ,   ASM.S "ARG"
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_COMP
                ,   mEqualsNJ ASM.D_COMP
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.M_PLUS_1

                    --push THIS
                ,   ASM.S "THIS"
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_COMP
                ,   mEqualsNJ ASM.D_COMP
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.M_PLUS_1

                    --push THAT
                ,   ASM.S "THAT"
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "SP"
                ,   aEqualsNJ ASM.M_COMP
                ,   mEqualsNJ ASM.D_COMP
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.M_PLUS_1

                    --ARG = SP-n-5
                ,   ASM.S "SP"
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S $ BS.pack (show nArgs)
                ,   dEqualsNJ ASM.D_MINUS_A
                ,   ASM.S "5"
                ,   dEqualsNJ ASM.D_MINUS_A
                ,   ASM.S "ARG"
                ,   mEqualsNJ ASM.D_COMP

                --LCL = SP
                ,   ASM.S "SP"
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "LCL"
                ,   mEqualsNJ ASM.D_COMP

                --goto f
                ,   ASM.S label
                ,   ASM.C ASM.ZERO ASM.NULL_DEST ASM.JMP
                ,   ASM.L retAddLabel ]

    convertCMD VM.RETURN =
        return  [   --FRAME = LCL
                    ASM.S "LCL"
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "R13"
                ,   mEqualsNJ ASM.D_COMP
                
                    --RET = *(FRAME-5)
                ,   ASM.S "5"
                ,   dEqualsNJ ASM.A_COMP
                ,   ASM.S "R13"
                ,   aEqualsNJ ASM.M_MINUS_D
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "R14"
                ,   mEqualsNJ ASM.D_COMP

                    -- *ARG = pop()
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.M_MINUS_1
                ,   aEqualsNJ ASM.M_COMP
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "ARG"
                ,   aEqualsNJ ASM.M_COMP
                ,   mEqualsNJ ASM.D_COMP

                    --SP = ARG+1
                ,   ASM.S "ARG"
                ,   dEqualsNJ ASM.M_PLUS_1
                ,   ASM.S "SP"
                ,   mEqualsNJ ASM.D_COMP

                    --THAT = *(FRAME-1)
                ,   ASM.S "R13"
                ,   aEqualsNJ ASM.M_MINUS_1
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "THAT"
                ,   mEqualsNJ ASM.D_COMP

                    --THIS = *(FRAME-2)\n"
                ,   ASM.S "2"
                ,   dEqualsNJ ASM.A_COMP
                ,   ASM.S "R13"
                ,   aEqualsNJ ASM.M_MINUS_D
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "THIS"
                ,   mEqualsNJ ASM.D_COMP

                    --ARG = *(FRAME-3)
                ,   ASM.S "3"
                ,   dEqualsNJ ASM.A_COMP
                ,   ASM.S "R13"
                ,   aEqualsNJ ASM.M_MINUS_D
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "ARG"
                ,   mEqualsNJ ASM.D_COMP


                    --LCL = *(FRAME-4)
                ,   ASM.S "4"
                ,   dEqualsNJ ASM.A_COMP
                ,   ASM.S "R13"
                ,   aEqualsNJ ASM.M_MINUS_D
                ,   dEqualsNJ ASM.M_COMP
                ,   ASM.S "LCL"
                ,   mEqualsNJ ASM.D_COMP

                    --goto RET
                ,   ASM.S "R14"
                ,   aEqualsNJ ASM.M_COMP
                ,   ASM.C ASM.ZERO ASM.NULL_DEST ASM.JMP ]